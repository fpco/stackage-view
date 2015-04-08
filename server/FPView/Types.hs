{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | All data types.

module FPView.Types where

import Control.Exception (Exception)
import Data.Set
import Data.Typeable (Typeable)
import Distribution.InstalledPackageInfo (PError)
import Distribution.Package
import Filesystem.Loc
import Filesystem.Path (FilePath)
import Language.Haskell.Extension
import Prelude hiding (FilePath)
import SharedTypes (TargetIdent)

-- | All exceptions thrown by the library.
data FPException
  = FPNoCabalFile FilePath
  | FPInvalidCabalFile (Loc Absolute File) PError
  deriving (Show,Typeable)
instance Exception FPException

data Target =
  Target {targetIdent :: TargetIdent
         ,targetFiles :: !(Set (Loc Relative File))
         ,targetExtensions :: ![Extension]
         ,targetDependencies :: ![PackageName]}
  deriving (Show)
