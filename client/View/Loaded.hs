{-# LANGUAGE OverloadedStrings #-}

-- | The project loaded successfully, show the two panes.

module View.Loaded where

import           Model
import           SharedTypes
import           Types

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Data.Function
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (pi)
import           React
import           React.Ace (Ace)
import qualified React.Ace as Ace
import           React.Internal
import           React.Lucid

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
