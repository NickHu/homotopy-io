module Homotopy.Webclient.State where

import Prelude
import Data.Lens (Lens', _Just)
import Data.Lens as Lens
import Data.Lens.At (at)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Homotopy.Core.Common (Generator)
import Homotopy.Webclient.Lenses as R

type State
  = { signature :: Signature
    , project :: Project
    , view :: Maybe View
    }

type Signature
  = { generators :: Map Generator GeneratorInfo
    }

type GeneratorInfo
  = { name :: String
    , color :: String
    }

type Project
  = { name :: String
    , abstract :: String
    }

-------------------------------------------------------------------------------
data View
  = ViewSignature
  | ViewProject
  | ViewUser

derive instance viewEq :: Eq View

derive instance viewOrd :: Ord View

-------------------------------------------------------------------------------
data Action
  = ToggleView View
  | RenameGenerator Generator String
  | RecolorGenerator Generator String
  | SelectGenerator Generator

-------------------------------------------------------------------------------
_generator :: Generator -> Lens' State (Maybe GeneratorInfo)
_generator id = R.signature <<< R.generators <<< at id

-------------------------------------------------------------------------------
initial :: State
initial =
  { signature:
      { generators: mempty
      }
  , project:
      { name: ""
      , abstract: ""
      }
  , view: Nothing
  }

-------------------------------------------------------------------------------
reduce :: Action -> State -> State
reduce action state = case action of
  --
  ToggleView view
    | state.view == Just view -> state { view = Nothing }
    | otherwise -> state { view = Just view }
  --
  RenameGenerator id name -> Lens.set (_generator id <<< _Just <<< R.name) name state
  --
  RecolorGenerator id color -> Lens.set (_generator id <<< _Just <<< R.color) color state
  --
  SelectGenerator id -> state
