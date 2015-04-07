{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Home page main entry point.

module Main where

import           React
import           React.Internal
import           React.Ace
import           React.Component

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.SwapQueue
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Function
import           Data.Functor.Identity
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
import           SharedTypes

#ifdef __GHCJS__
import           GHCJS.DOM
import           GHCJS.Types
import           GHCJS.Marshal
import           GHCJS.DOM.Types (Element (..), Event (..))
import           GHCJS.Foreign
#endif

--------------------------------------------------------------------------------
-- Types for the application

data AppReader =
  AppReader {appRunTypes :: (IO [(Span,Text)] -> IO (Maybe [(Span,Text)]))}

-- | The app state that is rendered to a view.
data State
  = StartingState
  | ChooseTargetsState !(Map TargetIdent Bool)
  | LoadingProjectState !LoadingStatus
  | LoadedState !Loaded
  | FailedState ![Either Text Error]
  deriving (Eq,Show)

-- | Loaded state.
data Loaded =
  Loaded {_loadedModules  :: ![Text]
         ,_loadedCurrent  :: !(Maybe (Text,Text,Int,Int,Int,Int))
         ,_loadedAce      :: !Ace
         ,_loadedTypeInfo :: !(Maybe (Maybe Span,Span,Text))
         ,_loadedMouseXY  :: !(Maybe (Int,Int))
         ,_loadedTargets  :: !(Map TargetIdent Bool)}
  deriving (Eq,Show)

$(makePrisms ''State)
$(makeLenses ''Loaded)

--------------------------------------------------------------------------------
-- Main application

-- | Grab the container element used for rendering into and start the
-- rendering loop.
main :: IO ()
main =
  do app <- getApp
     ace <- createAce app
     void (forkIO (loadingProcess app))
     container <-
       getElementById "react-container"
     react app (render ace) container

-- | Make the application.
getApp :: IO (App State (ReaderT AppReader IO))
getApp =
  do var <- newTVarIO StartingState
     ints <- newTVarIO 0
     cursors <- newTVarIO mempty
     enqueueTypes <- newSwapQueue
     let reader' =
           AppReader (makeRunner enqueueTypes)
         app =
           App var (flip runReaderT reader') ints cursors
     makeApp StartingState (flip runReaderT reader')

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
                    do setTVarIO (FailedState
                                    (concat [[Left "Bad HTTP status code from server."
                                             ,Left ("Status code was: " <>
                                                    T.pack (show status))]
                                            ,[Left "This probably means you stopped the server." | status ==
                                                                                                     0]]))
                                 var
                  DecodingError msg payload ->
                    setTVarIO (FailedState
                                 [Left "Unable to decode JSON from server. Consider this a bug."
                                 ,Left ("Decoding error was: " <> T.pack msg)
                                 ,Left ("Payload was: " <> payload)])
                              var
                  NoData ->
                    setTVarIO (FailedState [Left "No data from server when calling command. Please report this as a bug."])
                              var
              Right status ->
                case status of
                  AmbiguousTargets targets ->
                    do setTVarIO (ChooseTargetsState (M.fromList (map defaultChosenTarget targets)))
                                 var
                       loop False
                  LoadOK targets ms ->
                    do ace <- defaultAce
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
                       loop False)
      True

-- | Run the renderer in IO.
runIO :: IO a -> IO a
runIO = id

-- | Run the renderer purely.
runPure :: Identity a -> IO a
runPure = return . runIdentity

--------------------------------------------------------------------------------
-- Defaults

-- | Default loaded state.
defaultLoaded :: Map TargetIdent Bool -> [Text] -> Ace -> Loaded
defaultLoaded targets ms ace =
  Loaded ms Nothing ace Nothing Nothing targets

-- | Determine whether the target should be chosen by default or not.
defaultChosenTarget :: TargetIdent -> (TargetIdent, Bool)
defaultChosenTarget t =
  (t
  ,case t of
     LibraryIdent{} -> True
     _ -> False)

--------------------------------------------------------------------------------
-- Main rendering setup

-- | Our main view.
render :: (MonadReader AppReader m,MonadIO m)
       => Component State Ace m -> State -> ReactT State m ()
render ace state =
  case state of
    StartingState              -> starting
    LoadingProjectState prog   -> loading prog
    LoadedState lstate         -> loaded ace lstate
    FailedState errs           -> failed errs
    ChooseTargetsState targets -> chooseTargets targets

--------------------------------------------------------------------------------
-- Startup screen

starting :: Monad m
         => ReactT state m ()
starting =
  build "div"
        (do attr "className" "starting"
            centered (do build "img" (attr "src" "/static/img/starting.png")
                         build "p"
                               (do attr "className" "screen-info"
                                   "Starting ...")))

--------------------------------------------------------------------------------
-- Choose targets

chooseTargets :: Monad m
              => Map TargetIdent Bool -> ReactT State m ()
chooseTargets targets =
  build "div"
        (do attr "className" "choose-targets"
            centered (do build "img" (attr "src" "/static/img/starting.png")
                         build "div"
                               (do attr "className" "screen-info"
                                   build "h1" "Choose targets"
                                   build "p"
                                         (do attr "className" "choose-info"
                                             "You have more than one target in your Cabal package that requries a Main module, which would result in compilation conflicts. Below you can choose which targets to view.")
                                   build "ul"
                                         (do forM_ targetList (renderTarget targets)
                                             done (map fst (filter snd targetList))))))
  where targetList = M.toList targets

-- | Done choosing targets. Only proceed if at least one target was chosen.
done :: Monad m => [TargetIdent] -> ReactT State m ()
done idents =
  build "li"
        (do attr "className"
                 ("done " <>
                  if null idents
                     then "disabled"
                     else "enabled")
            "Done"
            onClick (\_ var -> call (ContinueWithTargets idents)))

-- | Render a target choice. Only one main-requiring target can be
-- chosen at once.
renderTarget :: Monad m
             => Map TargetIdent Bool -> (TargetIdent,Bool) -> ReactT State m ()
renderTarget targets (ident,chosen) =
  build "li"
        (do let unchoosable =
                  needsMain ident &&
                  any (\(ident',chosen') ->
                           ident' /= ident &&
                           needsMain ident' && chosen')
                      (M.toList targets)
            attr "className"
                 (if chosen
                     then "chosen"
                     else if unchoosable
                             then "unchooseable"
                             else "choosable")
            unless unchoosable
                   (onClick (const (modifyTVarIO _ChooseTargetsState (M.insert ident (not chosen)))))
            case ident of
              LibraryIdent -> "Library"
              ExecutableIdent name ->
                do build "strong" ("Executable")
                   " "
                   text name
              BenchmarkIdent name ->
                do build "strong" "Benchmark"
                   " "
                   text name
              TestSuiteIdent name ->
                do build "strong" "Test suite"
                   " "
                   text name)

--------------------------------------------------------------------------------
-- Loading screen

loading :: Monad m
        => LoadingStatus -> ReactT state m ()
loading prog =
  build "div"
        (do attr "className" "loading"
            centered (do build "img" (attr "src" "/static/img/loading.png")
                         case prog of
                           NotLoading ->
                             screenInfo "Initializing\8230"
                           Loading n total msg ->
                             do progressBar n total
                                screenInfo
                                  (text (T.pack (show n) <>
                                         "/" <>
                                         T.pack (show total) <>
                                         ": " <>
                                         msg))
                           _ ->
                             screenInfo "Done!"))
  where screenInfo inner =
          build "p"
                (do attr "className" "screen-info"
                    inner)

progressBar :: Monad m
            => Int -> Int -> ReactT state m ()
progressBar n total =
  build "div"
        (do attr "className" "prog-bar"
            build "div"
                  (do attr "className" "prog"
                      style "width"
                            (T.pack (show (round (width *
                                                  (n' / total')) :: Int)) <>
                             "px")))
  where width = 250 :: Double
        n' = fromIntegral n
        total' = fromIntegral total

--------------------------------------------------------------------------------
-- Failure screen

failed :: Monad m
       => [Either Text Error] -> ReactT state m ()
failed errs =
  build "div"
        (do attr "className" "failed"
            centered
                (do build "h1" "A problem occurred"
                    build "div"
                          (do attr "className" "screen-info"
                              forM_ errs
                                    (\e ->
                                       build "div"
                                             (case e of
                                                Left u ->
                                                  build "p" (text u)
                                                Right (Error fp span msg) ->
                                                  do build "p"
                                                           (do build "strong" (text fp)
                                                               text ":"
                                                               renderSpan span)
                                                     build "pre" (text msg))))))

-- | Render a span.
renderSpan :: Monad m => Span -> ReactT state m ()
renderSpan (Span sl sc el ec) =
    text
        (T.pack
             (show sl <> ":" <> show sc <> "-" <>
              show el <> ":" <> show ec))

--------------------------------------------------------------------------------
-- Loaded screen

loaded :: (MonadReader AppReader m,MonadIO m)
       => Component State Ace m -> Loaded -> ReactT State m ()
loaded ace Loaded{..} =
  build "div"
        (do attr "className" "loaded"
            build "table"
                  (build "tbody"
                         (build "tr"
                                (do build "td" (modules _loadedTargets _loadedModules _loadedCurrent)
                                    build "td" (pane ace _loadedCurrent))))
            case (,) <$> _loadedTypeInfo <*> _loadedMouseXY of
              Nothing -> return ()
              Just ((_,_,typ'),(x,y)) ->
                build "div"
                      (do attr "className" "type-popup"
                          do style "top"
                                   (T.pack (show (y + 14)) <>
                                    "px")
                             style "left"
                                   (T.pack (show x) <>
                                    "px")
                             build "div"
                                   (do attr "className" "expand"
                                       attr "title" "Expand selection"
                                       appreader <- ask
                                       onClick (expandSelection appreader)
                                       "\8596")
                             build "div"
                                   (do attr "className" "display"
                                       text typ')))
  where expandSelection _ _ var =
          do mloaded <- previewTVarIO _LoadedState var
             case mloaded of
               Just (Loaded _ _ _ ty xy _) ->
                 case (,) <$> ty <*> xy of
                   Just ((mparent,_,_),xy') ->
                     case mparent of
                       Nothing -> return ()
                       Just parent -> select xy' parent var
                   Nothing -> return ()
               _ -> return ()

-- | The code viewing pane.
pane :: (MonadReader AppReader m,MonadIO m)
     => Component State Ace m -> Maybe (t,Text,Int,Int,Int,Int) -> ReactT State m ()
pane ace mcur =
  do attr "className" "pane"
     case mcur of
       Just (_,contents,sl,sc,el,ec) ->
         buildComponent
           ace
           (_LoadedState . loadedAce)
           (do onDblClick (\_ var -> doubleClicked var)
               onAceClick
                 (\e var ->
                    do x <- aceClientX e
                       y <- aceClientY e
                       sl' <- aceSelectStartLine e
                       sc' <- aceSelectStartCol e
                       el' <- aceSelectEndLine e
                       ec' <- aceSelectEndCol e
                       select (x,y)
                              (Span sl' sc' el' ec')
                              var)
               attr "code" contents
               attr "start-line" (T.pack (show sl))
               attr "start-col" (T.pack (show sc))
               attr "end-line" (T.pack (show el))
               attr "end-col" (T.pack (show ec)))
       _ -> "Choose a module!"

-- | Jump to place definition on double click.
doubleClicked :: TVar State -> IO ()
doubleClicked var =
  do mloaded <- previewTVarIO _LoadedState var
     case mloaded of
       Just (Loaded ms mcur _ mspan _ _) ->
         case liftA2 (,) mcur mspan of
           Nothing -> return ()
           Just ((fp,_name,_,_,_,_),(_,sp,_)) ->
             do mloc <- call (GetIdentLocation fp sp)
                case mloc of
                  Nothing -> return ()
                  Just (Loc fp' line col) ->
                    viewModule ms fp' line col var
       _ -> return ()

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

-- | Module list.
modules :: Monad m
        => Map TargetIdent Bool
        -> [Text]
        -> Maybe (Text,t,a,a,a,a)
        -> ReactT State m ()
modules targets ms mcur =
  do attr "className" "module-list"
     build "p" (do attr "className" "switch-targets"
                   onClick (const (atomically . flip writeTVar (ChooseTargetsState targets)))
                   "Switch targets")
     build "ul"
           (forM_ ms
                  (\fp ->
                     build "li"
                           (do case mcur of
                                 Just (cur,_,_,_,_,_)
                                   | cur == fp ->
                                     attr "className" "current"
                                 _ -> return ()
                               onClick (const (viewModule ms fp 0 0))
                               text fp)))

viewModule :: [Text] -> Text -> Int -> Int -> TVar State -> IO ()
viewModule ms fp line col var =
  do contents <- call (GetModule fp)
     ace <- defaultAce
     modifyTVarIO
       _LoadedState
       (\l ->
          l {_loadedCurrent =
               (Just (fp,contents,line,col,line,col))
            ,_loadedTypeInfo = Nothing
            ,_loadedMouseXY = Nothing})
       var

--------------------------------------------------------------------------------
-- Utilities

-- | A full-screen centered thing.
centered :: Monad m
         => ReactT state m a -> ReactT state m a
centered inner =
  build "table"
        (build "tbody"
               (build "tr"
                      (build "td"
                             (do attr "className" "inner"
                                 build "div" inner))))

--------------------------------------------------------------------------------
-- TVar lens things

setTVarIO :: a -> TVar a -> IO ()
setTVarIO a v =
  atomically (writeTVar v a)

setTVarAt :: ASetter' s a -> a -> TVar s -> IO ()
setTVarAt l a v =
  atomically
    (modifyTVar v
                (set l a))

modifyTVarIO :: ASetter' s a -> (a -> a) -> TVar s -> IO ()
modifyTVarIO l f v =
  atomically
    (modifyTVar v
                (over l f))

viewTVarIO :: Getting a s a -> TVar s -> IO a
viewTVarIO g v =
  atomically
    (fmap (view g)
          (readTVar v))

previewTVarIO :: Getting (First a) s a -> TVar s -> IO (Maybe a)
previewTVarIO g v =
  atomically
    (fmap (preview g)
          (readTVar v))
