{-# LANGUAGE TemplateHaskell #-}
-- | Definition of AlexUserState
-- | It is used here to use micro lenses with it.
-- | All methods are public.
module AlexUserState where
import qualified Data.Map.Strict               as Map
import           Lens.Micro
import           Lens.Micro.TH
import           ParserData

data Value = Value { _dataType :: DataType
                   , _value :: Maybe Val -- ^ Nothing represents bottom or undefined
                   }
             deriving (Show, Eq, Ord, Read)

makeLenses ''Value

data AlexUserState = AlexUserState
  { -- | Definitions map. Contains all definition in a point
    _values :: [Map.Map String Value]
  , _definitions :: [Map.Map String DataDef]
  }
  deriving (Show, Eq, Ord, Read)

makeLenses ''AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState [Map.empty] [Map.empty]
