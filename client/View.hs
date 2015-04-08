{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Client-side view code.

module View where

import Types
import View.ChooseTargets
import View.Failed
import View.Loaded
import View.LoadingProject
import View.Starting

import Control.Monad.Reader
import Prelude hiding (pi)
import React
import React.Ace (Ace)
import React.Internal

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
