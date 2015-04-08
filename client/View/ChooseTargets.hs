{-# LANGUAGE OverloadedStrings #-}

-- | Target choosing screen.

module View.ChooseTargets where

import           Model
import           SharedTypes
import           Types
import           View.Template

import           Control.Monad
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           GHCJS.Yesod
import           React
import           React.Lucid

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
