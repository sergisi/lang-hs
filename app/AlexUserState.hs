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

-- | AlexUserState. It contains the state needed for the program
-- tempRefs contains an infinite list of temporal names. This makes a problem
-- to make the show and eq of this data. It was done at hand, only comparing
-- the first item, as the list is only consumed and not supplied.
data AlexUserState = AlexUserState
  { -- | Definitions map. Contains all definition in a point
    _values :: [Map.Map String Value]
  , _definitions :: [Map.Map String DataDef]
  , _tempRefs :: [String]
  }

makeLenses ''AlexUserState

instance Show AlexUserState where
  show a = "AlexUserState { _values = "
    ++ show (a ^. values)
    ++ ", _definitions = "
    ++ show (a ^. definitions)
    ++ ", _tempRefs = ["
    ++ show (head $ a ^. tempRefs)
    ++ ",  ...] } "

instance Eq AlexUserState where
  a == b = a ^. values == b ^. values
         && a ^.definitions == b ^. definitions
         && head (a ^. tempRefs) == head (b ^. tempRefs)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState [Map.empty] [Map.empty] $ map (("temp" ++ ) . show) [0..]
