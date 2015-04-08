{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module View where

import           Types
import           Model
import           SharedTypes

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           GHCJS.Yesod
import           Prelude hiding (pi)
import           React
import           React.Ace (Ace)
import qualified React.Ace as Ace
import           React.Internal
import           React.Lucid

-- | Our main view.
render :: (MonadIO m)
       => Component State Ace m -> State -> ReactT State m ()
render ace state =
  case state of
    StartingState -> starting
    LoadingProjectState prog -> loading prog
    LoadedState lstate -> loaded ace lstate
    FailedState errs -> failed errs
    ChooseTargetsState targets -> chooseTargets targets

-- | Start up screen.
starting :: Monad m
         => ReactT state m ()
starting =
  div_ (do class_ "starting"
           centered_ (do img_ (src_ "/static/img/starting.png")
                         p_ (do class_ "screen-info"
                                "Starting ...")))

-- | A full-screen centered thing.
centered_ :: Monad m
          => ReactT state m a -> ReactT state m a
centered_ inner =
  table_ (tbody_ (tr_ (td_ (do class_ "inner"
                               div_ inner))))

-- | Module list.
modules :: Monad m
        => Loaded
        -> ReactT State m ()
modules l =
  do class_ "module-list"
     p_ (do class_ "switch-targets"
            onClick (const (atomically .
                            flip writeTVar (ChooseTargetsState (_loadedTargets l))))
            "Switch targets")
     ul_ (forM_ (_loadedModules l)
                (\fp ->
                   li_ (do case _loadedCurrent l of
                             Just (cur,_,_,_,_,_)
                               | cur == fp ->
                                 class_ "current"
                             _ -> return ()
                           onClick (const (flip viewModule (Loc fp 0 0)))
                           text fp)))

-- | Choose from the list of targets.
chooseTargets :: Monad m
              => Map TargetIdent Bool -> ReactT State m ()
chooseTargets targets =
  div_ (do class_ "choose-targets"
           centered_ (do img_ (src_ "/static/img/starting.png")
                         div_ (do class_ "screen-info"
                                  h1_ "Choose targets"
                                  p_ (do class_ "choose-info"
                                         "You have more than one target in your Cabal package that requries a Main module, which would result in compilation conflicts. Below you can choose which targets to view.")
                                  ul_ (do forM_ targetList (renderTarget targets)
                                          done (map fst (filter snd targetList))))))
  where targetList = M.toList targets

-- | Done choosing targets. Only proceed if at least one target was chosen.
done :: Monad m => [TargetIdent] -> ReactT State m ()
done idents =
  li_ (do class_ ("done " <>
                  if null idents
                     then "disabled"
                     else "enabled")
          "Done"
          onClick (\_ _ ->
                     call (ContinueWithTargets idents)))

-- | Render a target choice. Only one main-requiring target can be
-- chosen at once.
renderTarget :: Monad m
             => Map TargetIdent Bool -> (TargetIdent,Bool) -> ReactT State m ()
renderTarget targets (ident,chosen) =
  li_ (do let unchoosable =
                needsMain ident &&
                any (\(ident',chosen') -> ident' /= ident && needsMain ident' &&
                                                             chosen')
                    (M.toList targets)
          class_ (if chosen
                     then "chosen"
                     else if unchoosable
                             then "unchooseable"
                             else "choosable")
          unless unchoosable
                 (onClick (const (modifyTVarIO _ChooseTargetsState
                                               (M.insert ident (not chosen)))))
          case ident of
            LibraryIdent -> "Library"
            ExecutableIdent name ->
              do strong_ ("Executable")
                 " "
                 text name
            BenchmarkIdent name ->
              do strong_ "Benchmark"
                 " "
                 text name
            TestSuiteIdent name ->
              do strong_ "Test suite"
                 " "
                 text name)

-- | The code viewing pane.
pane :: (MonadIO m)
     => Component State Ace m -> Maybe (t,Text,Int,Int,Int,Int) -> ReactT State m ()
pane ace mcur =
  do class_ "pane"
     case mcur of
       Just (_,contents,sl,sc,el,ec) ->
         buildComponent
           ace
           (_LoadedState . loadedAce)
           (do onDblClick (\_ var -> doubleClicked var)
               Ace.onClick
                (\e var ->
                  do x <- Ace.clientX e
                     y <- Ace.clientY e
                     sl' <- Ace.selectStartLine e
                     sc' <- Ace.selectStartCol e
                     el' <- Ace.selectEndLine e
                     ec' <- Ace.selectEndCol e
                     select (x,y)
                            (Span sl' sc' el' ec')
                            var)
               code_ contents
               Ace.startline_ sl
               Ace.startcol_ sc
               Ace.endline_ el
               Ace.endcol_ ec)
       _ -> "Choose a module!"

-- | Loading screen.
loading :: Monad m
        => LoadingStatus -> ReactT state m ()
loading prog =
  div_ (do class_ "loading"
           centered_ (do img_ (src_ "/static/img/loading.png")
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
          p_ (do class_ "screen-info"
                 inner)

-- | Progress bar display for loading screen.
progressBar :: Monad m
            => Int -> Int -> ReactT state m ()
progressBar n total =
  div_ (do class_ "prog-bar"
           div_ (do class_ "prog"
                    style "width"
                          (T.pack (show (round (width *
                                                (n' / total')) :: Int)) <>
                           "px")))
  where width = 250 :: Double
        n' = fromIntegral n
        total' = fromIntegral total

failed :: Monad m
       => [Either Text Error] -> ReactT state m ()
failed errs =
  div_ (do class_ "failed"
           centered_ (do h1_ "A problem occurred"
                         info))
  where info =
          div_ (do class_ "screen-info"
                   forM_ errs renderError)
        renderError e =
          div_ (case e of
                  Left u -> p_ (text u)
                  Right (Error fp sp msg) ->
                    do p_ (do strong_ (text fp)
                              text ":"
                              renderSpan sp)
                       pre_ (text msg))

-- | Render a span.
renderSpan :: Monad m => Span -> ReactT state m ()
renderSpan (Span sl sc el ec) =
  text (T.pack (show sl <> ":" <> show sc <> "-" <> show el <> ":" <> show ec))

-- | The loaded screen.
loaded :: (MonadIO m)
       => Component State Ace m -> Loaded -> ReactT State m ()
loaded ace l =
  div_ (do class_ "loaded"
           table_ (tbody_ (tr_ (do td_ (modules l)
                                   td_ (pane ace (_loadedCurrent l)))))
           case (,) <$> _loadedTypeInfo l <*> _loadedMouseXY l of
             Nothing -> return ()
             Just ((_,_,typ'),(x,y)) ->
               div_ (do class_ "type-popup"
                        style "top"
                              (T.pack (show (y + 14)) <>
                               "px")
                        style "left"
                              (T.pack (show x) <>
                               "px")
                        div_ (do class_ "expand"
                                 title_ "Expand selection"
                                 onClick (const expandSelection)
                                 "\8596")
                        div_ (do class_ "display"
                                 text typ')))
