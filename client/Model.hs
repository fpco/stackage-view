{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |

module Model where

import           SharedTypes
import           Types

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Exception
import qualified Control.Lens as Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as T
import           GHCJS.Yesod
import           Prelude hiding (pi)
import           React
import           React.Ace (Ace)
import qualified React.Ace as Ace
import           React.Internal

-- | Make the application.
getApp :: IO (App State IO)
getApp =
  makeApp StartingState id

-- | Run the loading process.
loadingProcess :: App State m -> IO ()
loadingProcess (App var _ _ _) =
  fix (\loop first ->
         do result <-
              try (call (PollLoading first))
            case result of
              Left ex ->
                case ex of
                  BadStatusCode status ->
                    badStatusCode status var
                  DecodingError msg payload ->
                    decodingError msg payload var
                  NoData -> noData var
              Right status ->
                loadResult loop status var)
      True

-- | When a load command came back with a result, handle the status.
loadResult :: (Bool -> IO ()) -> LoadingStatus -> TVar State -> IO ()
loadResult loop status var =
  case status of
    AmbiguousTargets targets ->
      do setTVarIO (ChooseTargetsState (M.fromList (map defaultChosenTarget targets))) var
         loop False
    LoadOK targets ms ->
      do ace <- Ace.getDef
         setTVarIO (LoadedState
                      (defaultLoaded (M.fromList targets)
                                     ms
                                     ace))
                   var
         loop False
    LoadFailed errs ->
      do setTVarIO (FailedState errs) var
         loop False
    _ ->
      do setTVarIO (LoadingProjectState status) var
         loop False

-- | No data from the server.
noData :: TVar State -> IO ()
noData =
  setTVarIO (FailedState
               [Left (mconcat ["No data from server when calling command. "
                              ,"Please report this as a bug."])])

-- | A JSON decoding error.
decodingError :: String -> Text -> TVar State -> IO ()
decodingError msg payload =
  setTVarIO (FailedState
               [Left "Unable to decode JSON from server. Consider this a bug."
               ,Left ("Decoding error was: " <> T.pack msg)
               ,Left ("Payload was: " <> payload)])

-- | HTTP server probably stopped.
badStatusCode :: Int -> TVar State -> IO ()
badStatusCode status =
  setTVarIO (FailedState (concat errors))
  where errors =
          [[Left "Bad HTTP status code from server."
           ,Left ("Status code was: " <>
                  T.pack (show status))]
          ,[Left "This probably means you stopped the server." | status == 0]]


-- | Jump to place definition on double click.
doubleClicked :: TVar State -> IO ()
doubleClicked var =
  do mloaded <- previewTVarIO _LoadedState var
     case do l <- mloaded
             (fp,_,_,_,_,_) <- _loadedCurrent l
             (_,sp,_) <- _loadedTypeInfo l
             return (fp,sp) of
       Nothing -> return ()
       Just (fp,sp) ->
         do mloc <- call (GetIdentLocation fp sp)
            maybe (return ())
                  (viewModule var)
                  mloc

-- | Select the given span and trigger a state update.
select :: (Int,Int) -> Span -> TVar State -> IO ()
select xy sp@(Span sl sc el ec) var =
  do mloaded <- previewTVarIO _LoadedState var
     case mloaded of
       Just l@(Loaded{_loadedCurrent = Just (fp,text',_,_,_,_)}) ->
         do unless (noChange (_loadedTypeInfo l))
                   (do spans <-
                         call (GetExpTypes fp
                                           (Span sl sc el ec))
                       setTVarAt _LoadedState
                                 ((case sortBy (on thinner fst) spans of
                                     ((child,typ):parents) ->
                                       l {_loadedCurrent =
                                            Just (fp,text',sl,sc,el,ec)
                                         ,_loadedTypeInfo =
                                            Just (listToMaybe (filter (/= child) (map fst parents))
                                                 ,child
                                                 ,typ)
                                         ,_loadedMouseXY =
                                            Just xy}
                                     _ ->
                                       let new =
                                             defaultLoaded (_loadedTargets l)
                                                           (_loadedModules l)
                                                           (_loadedAce l)
                                       in new {_loadedCurrent =
                                                 Just (fp,text',sl,sc,el,ec)
                                              ,_loadedMouseXY =
                                                 Just xy}))
                                 var)
       _ -> return ()
  where noChange Nothing = False
        noChange (Just (_,cur,_)) = cur == sp

-- | Is x thinner than y?
thinner :: Span -> Span -> Ordering
thinner x y =
  comparing (if on (==) spanSL x y &&
                on (==) spanEL x y
                then \(Span _ s _ e) -> e - s
                else \(Span s _ e _) -> e - s)
            x
            y

-- | View a module.
viewModule :: TVar State -> Loc -> IO ()
viewModule var (Loc fp line col) =
  do contents <- call (GetModule fp)
     modifyTVarIO
       _LoadedState
       (\l ->
          l {_loadedCurrent =
               (Just (fp,contents,line,col,line,col))
            ,_loadedTypeInfo = Nothing
            ,_loadedMouseXY = Nothing})
       var

-- | Expand the current selection.
expandSelection :: TVar State -> IO ()
expandSelection var =
  do mloaded <-
       previewTVarIO _LoadedState var
     case mloaded of
       Just (l::Loaded) ->
         case (,) <$> _loadedTypeInfo l <*> _loadedMouseXY l of
           Just ((mparent,_,_),xy') ->
             case mparent of
               Nothing -> return ()
               Just parent ->
                 select xy' parent var
           Nothing -> return ()
       _ -> return ()

--------------------------------------------------------------------------------
-- Defaults

-- | Default loaded state.
defaultLoaded :: Map TargetIdent Bool -> [Text] -> Ace -> Loaded
defaultLoaded targets ms ace =
  Loaded {_loadedModules = ms
         ,_loadedCurrent = Nothing
         ,_loadedAce = ace
         ,_loadedTypeInfo = Nothing
         ,_loadedMouseXY = Nothing
         ,_loadedTargets = targets}

-- | Determine whether the target should be chosen by default or not.
defaultChosenTarget :: TargetIdent -> (TargetIdent, Bool)
defaultChosenTarget t =
  (t
  ,case t of
     LibraryIdent{} -> True
     _ -> False)

--------------------------------------------------------------------------------
-- Tvar/lens helpers

setTVarIO :: a -> TVar a -> IO ()
setTVarIO a v = atomically (writeTVar v a)

setTVarAt :: Lens.ASetter' s a -> a -> TVar s -> IO ()
setTVarAt l a v =
  atomically
    (modifyTVar v
                (Lens.set l a))

modifyTVarIO :: Lens.ASetter' s a -> (a -> a) -> TVar s -> IO ()
modifyTVarIO l f v =
  atomically
    (modifyTVar v
                (Lens.over l f))

viewTVarIO :: Lens.Getting a s a -> TVar s -> IO a
viewTVarIO g v =
  atomically
    (fmap (Lens.view g)
          (readTVar v))

previewTVarIO :: Lens.Getting (First a) s a -> TVar s -> IO (Maybe a)
previewTVarIO g v =
  atomically
    (fmap (Lens.preview g)
          (readTVar v))
