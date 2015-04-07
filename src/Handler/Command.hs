{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |

module Handler.Command where

import           Control.Applicative ((<|>))
import           Control.Arrow
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Aeson (eitherDecodeStrict')
import           Data.Char
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.Text.IO as T
import           IdeSession
import           Import hiding (Content,FilePath,pi,check)
import           Prelude hiding (FilePath,writeFile,pi)
import           Session
import           SharedTypes

-- | Client command handler.
postCommandR :: Handler Value
postCommandR =
  do mtext <- lookupPostParam "json"
     bs <- maybe (invalidArgs ["json not provided"])
                 (return . encodeUtf8)
                 mtext
     command <- either (invalidArgs . return . T.pack)
                       return
                       (eitherDecodeStrict' bs)
     let render :: ToJSON a
                => Returns a -> a -> Handler Value
         render Returns x = returnJson x
     case command of
       PollLoading first r -> pollLoading first >>= render r
       GetModule n r -> getModule n >>= render r
       GetExpTypes fp span r -> expTypes fp span >>= render r
       GetIdentLocation fp span r -> getIdentLoc fp span >>= render r
       ContinueWithTargets ts r -> continueWithTargets ts >>= render r

-- | Continue the session with the given targets.
continueWithTargets :: [TargetIdent] -> Handler ()
continueWithTargets targets =
  do app <- getYesod
     liftIO (continueWith targets app)

-- | Poll for status updated.
pollLoading :: Bool -> Handler LoadingStatus
pollLoading first =
  do app <- getYesod
     status <- liftIO (atomically
                         (do new <- readTVar (appLoading app)
                             last <- readTVar (appLastLoading app)
                             unless first (check (new /= last))
                             writeTVar (appLastLoading app) new
                             return new))
     return status

-- | Get the contents of the module.
getModule :: Text -> Handler Text
getModule fp =
  liftIO (T.readFile (T.unpack fp))

-- | Get the location of the ident at the given span.
getIdentLoc :: Text -> Span -> Handler (Maybe Loc)
getIdentLoc fp span =
  do sess <- fmap appSession getYesod
     get <- liftIO (getSpanInfo sess)
     mn <-
       fmap (fromMaybe "Main")
            (liftIO (guessModuleName fp))
     return (bind (extract . getIdInfo . snd)
                  (listToMaybe (get mn (toSourceSpan fp span))))
  where extract :: IdInfo -> Maybe Loc
        extract i =
          toLoc (idDefSpan (idProp i)) <|>
          (case idScope i of
             Imported _from span _qual -> toLoc span
             _ -> Nothing)
        toLoc s =
          case s of
            ProperSpan s ->
              Just (Loc (T.pack (spanFilePath s))
                        (spanFromLine s)
                        (spanFromColumn s))
            TextSpan{} -> Nothing
        getIdInfo s =
          case s of
            SpanId i -> i
            SpanQQ i -> i
        bind = (=<<)

-- | Get expression types.
expTypes :: Text -> Span -> Handler [(Span,Text)]
expTypes fp span =
  do sess <- fmap appSession getYesod
     get <- liftIO (getExpTypes sess)
     liftIO (putStrLn ("getExpTypes " ++
                       show "Handler.Command" ++
                       " " ++
                       show (toSourceSpan fp span)))
     mn <-
       fmap (fromMaybe "Main")
            (liftIO (guessModuleName fp))
     return (map (first toSpan)
                 (get mn (toSourceSpan fp span)))

-- | Convert to an ide-backend 'SourceSpan'.
toSourceSpan :: Text -> Span -> SourceSpan
toSourceSpan fp (Span sl sc el ec) =
  SourceSpan (T.unpack fp)
             sl
             sc
             el
             ec

-- | Convert from an ide-backend 'SourceSpan'.
toSpan :: SourceSpan -> Span
toSpan (SourceSpan _ sl sc el ec) =
  Span sl sc el ec

-- | Guess the module name of the file by parsing its lines until
-- @module Foo@ is found. This isn't complete but generally should be
-- effective. @module {- bar -} Foo@ won't work but this is a stop-gap
-- anyway. Edsko tells me that requiring the module name in this call
-- is actually a mistake anyway -- we should only need to use the
-- filepath.
guessModuleName :: Text -> IO (Maybe Text)
guessModuleName fp =
  runResourceT
    (CB.sourceFile (T.unpack fp) $=
     CT.decodeUtf8 $=
     CT.lines $$
     consumePrefix)
  where consumePrefix =
          do mheader <- await
             case mheader of
               Nothing ->
                 return Nothing
               Just line
                 | Just rest <-
                    T.stripPrefix "module " line -> consumeName rest
                 | otherwise -> consumePrefix
        consumeName (T.strip -> string) =
          if probablyModuleName string
             then return (Just (takeModuleName string))
             else do mnextLine <- await
                     case mnextLine of
                       Nothing -> return Nothing
                       Just line -> consumeName line
        probablyModuleName =
          T.all isUpper .
          T.take 1
        takeModuleName = T.takeWhile valid
          where valid c =
                  isLetter c ||
                  isDigit c ||
                  elem c "._'"
