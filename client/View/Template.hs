{-# LANGUAGE OverloadedStrings #-}

-- | Shared template code.

module View.Template where

import Control.Monad
import Prelude hiding (pi)
import React
import React.Lucid

-- | A full-screen centered thing.
centered_ :: Monad m
          => ReactT state m a -> ReactT state m a
centered_ inner =
  table_ (tbody_ (tr_ (td_ (do class_ "inner"
                               div_ inner))))
