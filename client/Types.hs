{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Types for the application.

module Types where

import qualified Control.Lens as Lens
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Text (Text)
import           Prelude hiding (pi)
import           React.Ace (Ace)
import           SharedTypes

-- | The app state that is rendered to a view.
data State
  = StartingState
  | ChooseTargetsState !(Map TargetIdent Bool)
  | LoadingProjectState !LoadingStatus
  | LoadedState !Loaded
  | FailedState ![Either Text Error]
  deriving (Eq,Show)

-- | Loaded state.
data Loaded =
  Loaded {_loadedModules  :: ![Text]
         ,_loadedCurrent  :: !(Maybe (Text,Text,Int,Int,Int,Int))
         ,_loadedAce      :: !Ace
         ,_loadedTypeInfo :: !(Maybe (Maybe Span,Span,Text))
         ,_loadedMouseXY  :: !(Maybe (Int,Int))
         ,_loadedTargets  :: !(Map TargetIdent Bool)}
  deriving (Eq,Show)

$(Lens.makePrisms ''State)
$(Lens.makeLenses ''Loaded)
