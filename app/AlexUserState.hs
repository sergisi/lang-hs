{-# LANGUAGE TemplateHaskell #-}
-- | Definition of AlexUserState
-- | It is used here to use micro lenses with it.
module AlexUserState (AlexUserState (..), alexInitUserState, integers, reals) where
import qualified Data.Map.Strict as Map
import Lens.Micro
import Lens.Micro.TH

data AlexUserState = AlexUserState
  { -- | Integers map. Contains all definition in a point
    _integers :: Map.Map Char Int,
    -- | Reals map. Contains all real definitions in a point.
    _reals :: Map.Map Char Double
  }
  deriving (Show, Eq, Ord, Read)

makeLenses ''AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState Map.empty Map.empty
