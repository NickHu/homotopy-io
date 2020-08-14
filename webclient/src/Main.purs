module Homotopy.Webclient.Main where

import Prelude
import Data.Foldable (fold, intercalate, sequence_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Exception (throw)
import Foreign.Object as Object
import Homotopy.Core.Common (Boundary(..), Generator(..), SliceIndex(..), Height(..))
import Homotopy.Core.Diagram (Diagram(..))
import Homotopy.Core.Diagram as Diagram
import Homotopy.Webclient.Components.Diagram (makeDiagram)
import Homotopy.Webclient.Components.Icon (icon, iconButton)
import Homotopy.Webclient.Components.PanZoom (makePanZoom)
import Homotopy.Webclient.Components.Transition (makeSwitch)
import Homotopy.Webclient.State (Action(..), GeneratorInfo)
import Homotopy.Webclient.State as State
import Partial.Unsafe (unsafePartial)
import React.Basic (JSX)
import React.Basic (ReactComponent, elementKeyed) as React
import React.Basic.DOM as D
import React.Basic.DOM.Events (preventDefault)
import React.Basic.DOM.Internal (css)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

foreign import logoSVG :: String

main :: Effect Unit
main = do
  container <-
    ( window
        >>= document
        >>> map HTMLDocument.toNonElementParentNode
        >>= getElementById "app"
        >>= maybe (throw "Missing #app element.") pure
    )
  app <- makeApp
  D.render (app {}) container

makeApp :: React.Component {}
makeApp = do
  reducer <- React.mkReducer \state action -> State.reduce action state
  mainDrawer <- makeMainDrawer
  workspace <- makeWorkspace
  React.component "App" \_ -> React.do
    state /\ dispatch <- React.useReducer State.initial reducer
    shortcuts <-
      React.useMemo unit \_ ->
        Map.fromFoldable
          [ "i" /\ dispatch IdentityDiagram
          , "c" /\ dispatch ClearWorkspace
          , "s" /\ dispatch (MakeBoundary Source)
          , "t" /\ dispatch (MakeBoundary Target)
          , "r" /\ dispatch RestrictDiagram
          ]
    useKeyboardShortcuts shortcuts
    let
      store = { dispatch, state }
    pure
      $ fold
          [ sidebar { store }
          , mainDrawer { store }
          , workspace { store }
          ]

div :: String -> Array JSX -> JSX
div className children = D.div { className, children }

-------------------------------------------------------------------------------
useKeyboardShortcuts :: Map String (Effect Unit) -> React.Hook _ Unit
useKeyboardShortcuts shortcuts =
  -- By using `UnsafeReference` the event listener is only updated when one of
  -- the handlers is different by reference equality.
  React.useEffect (map React.UnsafeReference shortcuts) do
    target <- window >>= document >>> map HTMLDocument.toEventTarget
    listener <-
      eventListener \event ->
        sequence_ do
          keyboardEvent <- KeyboardEvent.fromEvent event
          Map.lookup (KeyboardEvent.key keyboardEvent) shortcuts
    addEventListener (EventType "keydown") listener false target
    pure (removeEventListener (EventType "keydown") listener false target)

-------------------------------------------------------------------------------
type WorkspaceProps
  = { store :: State.Store
    }

makeWorkspace :: React.Component WorkspaceProps
makeWorkspace = do
  panZoom <- makePanZoom
  diagramView <- makeDiagramView
  pure \{ store } -> case store.state.workspace of
    Nothing -> mempty
    Just { path } ->
      div "workspace"
        [ div "workspace__breadcrumbs" [ breadcrumbs { store, path } ]
        , div "workspace__diagram" [ panZoom { children: [ diagramView { store } ] } ]
        ]

breadcrumbs :: { path :: List SliceIndex, store :: State.Store } -> JSX
breadcrumbs props = div classNames ([ homeBreadcrumb ] <> pathBreadcrumbs)
  where
  pathLength = List.length props.path

  classNames =
    intercalate " "
      [ "slice-breadcrumbs"
      , guard (pathLength == 0) "slice-breadcrumbs--empty"
      ]

  homeBreadcrumb =
    D.div
      { className: "slice-breadcrumbs__breadcrumb slice-breadcrumbs__breadcrumb--home"
      , onClick: handler_ (props.store.dispatch (AscendSlice pathLength))
      , children:
          [ icon
              { name: "layers"
              , className: "slice-breadcrumbs__breadcrumb__home"
              }
          ]
      }

  pathBreadcrumbs = List.toUnfoldable $ List.mapWithIndex breadcrumb $ List.reverse props.path

  breadcrumb i height =
    fold
      [ icon
          { name: iconForHeight height
          , className: "slice-breadcrumbs__separator"
          }
      , D.div
          { className: "slice-breadcrumbs__breadcrumb " <> classNameForIndex i
          , onClick: handler_ (actionForIndex i)
          , children: [ D.text (labelForHeight height) ]
          }
      ]

  actionForIndex i =
    if i == pathLength - 1 then
      pure unit
    else
      props.store.dispatch (AscendSlice (pathLength - i - 1))

  iconForHeight = case _ of
    Boundary _ -> "chevrons-right"
    Interior (Singular _) -> "chevrons-right"
    Interior (Regular _) -> "chevron-right"

  labelForHeight = case _ of
    Boundary Source -> "S"
    Boundary Target -> "T"
    Interior (Singular i) -> show i
    Interior (Regular i) -> show i

  classNameForIndex i =
    if i == pathLength - 1 then
      "slice-breadcrumbs__breadcrumb--active"
    else
      "slice-breadcrumbs__breadcrumb--inactive"

-------------------------------------------------------------------------------
type SidebarProps
  = { store :: State.Store
    }

sidebar :: SidebarProps -> JSX
sidebar { store } =
  div "sidebar"
    [ div "sidebar__logo" [ logo ]
    , div "sidebar__actions"
        [ action { view: State.ViewProject, icon: "book", name: "Project" }
        , action { view: State.ViewSignature, icon: "box", name: "Signature" }
        , action { view: State.ViewUser, icon: "user", name: "User" }
        ]
    ]
  where
  logo =
    D.img
      { src: logoSVG
      , className: "sidebar__logo__image"
      , alt: "homotopy.io"
      , title: "homotopy.io"
      }

  tooltip name children =
    D.div
      { className: "tooltip tooltip--right"
      , _data: Object.fromFoldable [ "tooltip" /\ name ]
      , children
      }

  action { view, icon: iconName, name } =
    D.div
      { className: "sidebar__action"
      , onClick: handler preventDefault \_ -> store.dispatch (ToggleView view)
      , children:
          [ tooltip name
              [ icon
                  { name: iconName
                  , className: "sidebar__action__icon"
                  }
              ]
          ]
      }

-------------------------------------------------------------------------------
type DrawerProps
  = { title :: String
    , className :: String
    , content :: Array JSX
    , actions :: Array DrawerActionProps
    }

type DrawerActionProps
  = { label :: String
    , onClick :: Effect Unit
    , icon :: String
    }

drawer :: DrawerProps -> JSX
drawer { title, className, content, actions } =
  div ("drawer " <> className)
    [ div "drawer__header"
        [ div "drawer__title" [ D.text title ]
        , div "drawer__actions" (map action actions)
        ]
    , div "drawer__content" content
    ]
  where
  action { label, onClick, icon } =
    iconButton
      { label
      , onClick
      , icon
      , className: "drawer__action"
      }

-------------------------------------------------------------------------------
makeMainDrawer :: React.Component { store :: State.Store }
makeMainDrawer = do
  signatureDrawer <- makeSignatureDrawer
  userDrawer <- makeUserDrawer
  projectDrawer <- makeProjectDrawer
  switch <- makeSwitch
  pure \{ store } ->
    switch
      { timeout: 150.0
      , className: "drawer"
      , children:
          store.state.view
            <#> case _ of
                State.ViewSignature -> signatureDrawer { store }
                State.ViewProject -> projectDrawer { store }
                State.ViewUser -> userDrawer { store }
      }

-------------------------------------------------------------------------------
makeSignatureDrawer :: React.Component { store :: State.Store }
makeSignatureDrawer = do
  signature <- makeSignature
  React.component "SignatureDrawer" \{ store } ->
    pure
      $ drawer
          { title: "Signature"
          , className: "signature"
          , content: [ signature { store } ]
          , actions:
              [ { label: "Add Generator"
                , onClick: store.dispatch MakeGenerator
                , icon: "plus"
                }
              ]
          }

makeSignature :: React.Component { store :: State.Store }
makeSignature = do
  generator <- makeGenerator
  React.component "Signature" \{ store } ->
    pure
      $ D.ul
          { className: "signature__generators"
          , children:
              [ foldMapWithIndex
                  ( \id info ->
                      React.elementKeyed generator
                        { key: show id
                        , generator: id
                        , info
                        , store
                        }
                  )
                  store.state.signature.generators
              ]
          }

type GeneratorProps
  = { generator :: Generator
    , info :: GeneratorInfo
    , store :: State.Store
    }

makeGenerator :: Effect (React.ReactComponent GeneratorProps)
makeGenerator =
  React.reactComponent "Generator" \{ generator, info, store } ->
    pure
      $ D.li
          { className: "generator"
          , onClick: handler_ $ store.dispatch (SelectGenerator generator)
          , children:
              [ D.div
                  { className: "generator__color"
                  , style:
                      D.css
                        { background: info.color
                        }
                  }
              , div "generator__name" [ D.text info.name ]
              , div "generator__actions"
                  [ iconButton
                      { onClick: pure unit
                      , label: "Edit"
                      , icon: "edit"
                      , className: ""
                      }
                  , iconButton
                      { onClick: store.dispatch (RemoveGenerator generator)
                      , label: "Remove"
                      , icon: "trash"
                      , className: ""
                      }
                  ]
              ]
          }

-------------------------------------------------------------------------------
makeProjectDrawer :: React.Component { store :: State.Store }
makeProjectDrawer = do
  React.component "ProjectDrawer" \{ store } ->
    pure
      $ drawer
          { title: "Project"
          , className: "project"
          , content: []
          , actions: []
          }

-------------------------------------------------------------------------------
makeUserDrawer :: React.Component { store :: State.Store }
makeUserDrawer = do
  React.component "UserDrawer" \{ store } ->
    pure
      $ drawer
          { title: "User"
          , className: "user"
          , content: []
          , actions: []
          }

-------------------------------------------------------------------------------
makeDiagramView :: React.Component { store :: State.Store }
makeDiagramView = do
  diagramComp <- makeDiagram
  pure \{ store } -> case store.state.workspace of
    Nothing -> mempty
    Just { diagram, path } ->
      diagramComp
        { id: "diagram"
        , scale: { x: 50.0, y: 50.0 }
        , style2d:
            { pointRadius: 4.0
            , wireThickness: 3.0
            , crossingThickness: 6.0
            }
        , style1d:
            { pointRadius: 4.0
            , wireThickness: 3.0
            }
        , style0d:
            { pointRadius: 4.0
            }
        , colors: map _.color store.state.signature.generators
        , diagram: unsafePartial $ fromJust $ followPath (List.reverse path) diagram
        , onSliceSelect: \index -> store.dispatch (DescendSlice index)
        , onClick: logShow
        }
  where
  followPath = case _, _ of
    Nil, diagram -> Just diagram
    i : is, DiagramN diagram -> Diagram.sliceAt diagram i >>= followPath is
    _ : _, Diagram0 _ -> Nothing
