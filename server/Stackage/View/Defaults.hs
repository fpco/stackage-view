{-# LANGUAGE OverloadedStrings #-}

-- | Default values used in the project.

module Stackage.View.Defaults where

import Data.Text (Text)

-- | Extensions used for Haskell files.
haskellFileExts :: [Text]
haskellFileExts = ["hs","hsc","lhs"]
