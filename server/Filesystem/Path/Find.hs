{-# LANGUAGE DataKinds #-}

-- | Finding files.

module Filesystem.Path.Find where

import Data.List
import Data.Maybe
import Filesystem
import Filesystem.Loc as FL
import Prelude hiding (FilePath)

-- | Find the location of a file matching the given predicate.
findFileUp :: Loc Absolute Dir                -- ^ Start here.
           -> (Loc Absolute File -> Bool)     -- ^ Predicate to match the file.
           -> Maybe (Loc Absolute Dir)        -- ^ Do not ascend above this directory.
           -> IO (Maybe (Loc Absolute File))  -- ^ Absolute file path.
findFileUp dir p upperBound =
  do files <- listDirectory (FL.toFilePath dir)
     case find p (mapMaybe parseAbsoluteFileLoc files) of
       Just path -> return (Just path)
       Nothing ->
         if Just dir ==
            upperBound
            then return Nothing
            else if FL.parentOfDir dir == dir
                    then return Nothing
                    else findFileUp (FL.parentOfDir dir)
                                    p
                                    upperBound
