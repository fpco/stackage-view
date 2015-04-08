{-# LANGUAGE DeriveGeneric #-}

-- | Types shared between client and server.

module SharedTypes where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics

-- | Trick for linking return type to request type.
data Returns a = Returns
    deriving (Generic)
instance ToJSON (Returns a)
instance FromJSON (Returns a)

-- | An identifier, used for choosing the target.
data TargetIdent
  = LibraryIdent
  | ExecutableIdent !Text
  | BenchmarkIdent !Text
  | TestSuiteIdent !Text
  deriving (Show,Eq,Ord)

deriveJSON defaultOptions ''TargetIdent

-- | Source span.
data Span =
  Span {spanSL :: !Int
       ,spanSC :: !Int
       ,spanEL :: !Int
       ,spanEC :: !Int}
  deriving (Show,Eq)

deriveJSON defaultOptions ''Span

-- | Some error message.
data Error =
  Error {errorFile :: !Text
        ,errorSpan :: !Span
        ,errorMsg :: !Text}
  deriving (Eq,Show)

deriveJSON defaultOptions ''Error

-- | Status for loading the project.
data LoadingStatus
  = NotLoading
  | Loading !Int !Int !Text
  | LoadOK ![(TargetIdent,Bool)] ![Text]
  | LoadFailed ![Either Text Error]
  | AmbiguousTargets ![TargetIdent]
  deriving (Eq,Show)

deriveJSON defaultOptions ''LoadingStatus

-- | Source location.
data Loc =
  Loc {locFile :: !Text
      ,locLine :: !Int
      ,locCol  :: !Int}
  deriving (Show,Eq)

deriveJSON defaultOptions ''Loc

-- | Client comands.
data Command
    = PollLoading !Bool (Returns LoadingStatus)
    | GetModule !Text (Returns Text)
    | GetExpTypes !Text !Span (Returns [(Span,Text)])
    | GetIdentLocation !Text !Span (Returns (Maybe Loc))
    | ContinueWithTargets ![TargetIdent] (Returns ())

deriveJSON defaultOptions ''Command

-- | Does the target require a Main module?
needsMain :: TargetIdent -> Bool
needsMain i =
  case i of
    LibraryIdent -> False
    ExecutableIdent _ -> True
    BenchmarkIdent _ -> True
    TestSuiteIdent _ -> True
