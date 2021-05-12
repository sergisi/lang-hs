{-# LANGUAGE TemplateHaskell #-}
-- | Definition of AlexUserState
-- | It is used here to use micro lenses with it.
-- | All methods are public.
module AlexUserState where
import qualified Data.Map.Strict               as Map
import           Lens.Micro
import           Lens.Micro.TH

data TypeDefinition = DBool
                | DInt
                | DChar
                | DUnit
                | DNothing
                | DFunction [TypeDefinition]
                | DMult [TypeDefinition]         -- ^ Mult Type. Maybe use a Map.String Definition
                | DSum (Map.Map String TypeDefinition) -- ^ Sum Type
                deriving (Ord, Eq, Show, Read)

data ValueDefinition = VBool Bool
      | VInt Int
      | VChar Char
      deriving (Ord, Eq, Show, Read)

data Definition = Definition { _type :: TypeDefinition
                             , _value :: Maybe ValueDefinition -- ^ Nothing represents bottom or undefined
                             }
                | TypeD { _typeD :: TypeDefinition }

newtype AlexUserState = AlexUserState
  { -- | Definitions map. Contains all definition in a point
    _definitions :: [Map.Map String Definition]
  }

makeLenses ''AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState [Map.empty]

data ABC = A | B | C Int Bool
