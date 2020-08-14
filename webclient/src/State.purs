module Homotopy.Webclient.State where

import Prelude
import Data.Array as Array
import Data.Foldable (all, foldr)
import Data.Lens (Lens', Traversal', _Just)
import Data.Lens as Lens
import Data.Lens.At (at)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Homotopy.Core.Common (Boundary(..), Generator(..), Height(..), SliceIndex(..))
import Homotopy.Core.Diagram (Diagram(..), DiagramN)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Webclient.Lenses as R
import Partial.Unsafe (unsafePartial)

type State
  = { signature :: Signature
    , project :: Project
    , view :: Maybe View
    , workspace :: Maybe Workspace
    , boundary :: Maybe (Boundary /\ Diagram)
    }

type Workspace
  = { diagram :: Diagram
    , path :: List SliceIndex
    }

type Signature
  = { generators :: Map Generator GeneratorInfo
    }

type GeneratorInfo
  = { name :: String
    , color :: String
    , diagram :: Diagram
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
  | RemoveGenerator Generator
  | SelectGenerator Generator
  | DescendSlice SliceIndex
  | AscendSlice Int
  | ClearWorkspace
  | IdentityDiagram
  | MakeBoundary Boundary
  | MakeGenerator
  | RestrictDiagram

-------------------------------------------------------------------------------
_generator :: Generator -> Lens' State (Maybe GeneratorInfo)
_generator id = R.signature <<< R.generators <<< at id

_path :: Traversal' State (List SliceIndex)
_path = R.workspace <<< _Just <<< R.path

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
  , workspace: Nothing
  , boundary: Nothing
  }

-------------------------------------------------------------------------------
reduce :: Action -> State -> State
reduce action = case action of
  --
  ToggleView view -> \state ->
    if state.view == Just view then
      state { view = Nothing }
    else
      state { view = Just view }
  --
  RenameGenerator id name -> Lens.set (_generator id <<< _Just <<< R.name) name
  --
  RecolorGenerator id color -> Lens.set (_generator id <<< _Just <<< R.color) color
  -- todo: when removing a generator, remove and clear everything that depends on it.
  RemoveGenerator id -> Lens.set (_generator id) Nothing
  --
  SelectGenerator id -> selectGenerator id
  --
  DescendSlice index -> Lens.over _path (index : _)
  --
  AscendSlice count -> Lens.over _path (List.drop count)
  --
  ClearWorkspace -> _ { workspace = Nothing }
  --
  IdentityDiagram ->
    Lens.over (R.workspace <<< _Just) \workspace ->
      { diagram: DiagramN (Diagram.identity workspace.diagram)
      , path: workspace.path
      }
  --
  MakeGenerator -> makeGenerator Nothing
  --
  MakeBoundary boundary -> makeBoundary boundary
  --
  RestrictDiagram -> restrictDiagram

-------------------------------------------------------------------------------
nextGenerator :: Int -> State -> Generator
nextGenerator dimension state = Generator { id: go 0, dimension }
  where
  ids = Set.map (\(Generator g) -> g.id) $ Map.keys $ state.signature.generators

  go id = if not (id `Set.member` ids) then id else go (id + 1)

-------------------------------------------------------------------------------
selectGenerator :: Generator -> State -> State
selectGenerator id state = case state.workspace, Lens.view (_generator id) state of
  Nothing, Just info ->
    state
      { workspace =
        Just
          { path: Nil
          , diagram: info.diagram
          }
      }
  _, _ -> state

-------------------------------------------------------------------------------
baseColors :: Array String
baseColors =
  [ "#2980b9" -- belize blue
  , "#c0392b" -- pomegranate
  , "#f39c12" -- orange
  , "#8e44ad" -- wisteria
  , "#27ae60" -- nephritis
  , "#f1c40f" -- sunflower
  , "#ffffff" -- white
  , "#000000" -- black
  ]

makeGenerator :: Maybe (Diagram /\ Diagram) -> State -> State
makeGenerator boundary state = Lens.setJust (_generator generator) info state
  where
  generator@(Generator { id }) = nextGenerator 0 state

  name = "Cell " <> show id

  color = unsafePartial $ fromJust $ baseColors Array.!! (id `mod` Array.length baseColors)

  diagram = case boundary of
    Nothing -> Diagram0 generator
    Just (source /\ target) -> DiagramN (Diagram.fromGenerator source target generator)

  info = { name, color, diagram }

makeBoundary :: Boundary -> State -> State
makeBoundary boundary state = case state.workspace of
  Nothing -> state
  Just workspace -> case boundary, state.boundary of
    Source, Just (Target /\ target) ->
      state
        # _ { workspace = Nothing, boundary = Nothing }
        # makeGenerator (Just (workspace.diagram /\ target))
    Target, Just (Source /\ source) ->
      state
        # _ { workspace = Nothing, boundary = Nothing }
        # makeGenerator (Just (source /\ workspace.diagram))
    _, _ ->
      state
        { workspace = Nothing
        , boundary = Just (boundary /\ workspace.diagram)
        }

-------------------------------------------------------------------------------
restrictDiagram :: State -> State
restrictDiagram =
  Lens.over (R.workspace <<< _Just) \workspace ->
    let
      isNonSingular = case _ of
        Interior (Singular _) -> false
        _ -> true

      sliceAt i d = unsafePartial $ fromJust $ Diagram.sliceAt (Diagram.toDiagramN d) i
    in
      if all isNonSingular workspace.path then
        { diagram: foldr sliceAt workspace.diagram workspace.path, path: Nil }
      else
        workspace

-------------------------------------------------------------------------------
type Store
  = { state :: State
    , dispatch :: Action -> Effect Unit
    }
