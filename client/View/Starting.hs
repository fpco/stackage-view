{-# LANGUAGE OverloadedStrings #-}

-- | Start up screen. Very brief.

module View.Starting where

import React
import React.Lucid
import View.Template

-- | Start up screen.
starting :: Monad m
         => ReactT state m ()
starting =
  div_ (do class_ "starting"
           centered_ (do img_ (src_ "/static/img/starting.png")
                         p_ (do class_ "screen-info"
                                "Starting ...")))
