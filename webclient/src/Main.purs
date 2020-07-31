module Homotopy.Webclient.Main where

import Prelude
import Data.Foldable (fold)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import Foreign.Object as Object
import Homotopy.Core.Common (Generator(..), Boundary(..))
import Homotopy.Core.Diagram (Diagram(..), DiagramN)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Layout as Layout
import Homotopy.Webclient.Components.PanZoom (makePanZoom)
import Homotopy.Webclient.Components.Diagram (makeDiagram)
import Homotopy.Webclient.Components.Transition (makeSwitch, cssTransition, transitionClasses)
import Homotopy.Webclient.Diagram2D (diagramSVG)
import Homotopy.Webclient.State (State, Action(..))
import Homotopy.Webclient.State as State
import Partial.Unsafe (unsafePartial)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM as D
import React.Basic.DOM.Events (preventDefault)
import React.Basic.DOM.SVG as SVG
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Data.Map as Map

foreign import logoSVG :: String

foreign import featherIcons :: String

main :: Effect Unit
main = do
  container <-
    ( window
        >>= document
        >>> map toNonElementParentNode
        >>= getElementById "app"
        >>= maybe (throw "Missing #app element.") pure
    )
  app <- makeApp
  D.render (app {}) container

makeApp :: React.Component {}
makeApp = do
  reducer <- React.mkReducer State.reduce
  mainDrawer <- makeMainDrawer
  workspace <- makeWorkspace
  React.component "App" \_ -> React.do
    state /\ dispatch <- React.useReducer State.initial reducer
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
type WorkspaceProps
  = { store :: State.Store
    }

makeWorkspace :: React.Component WorkspaceProps
makeWorkspace = do
  panZoom <- makePanZoom
  diagramView <- makeDiagramView
  pure \props ->
    div "workspace"
      [ panZoom
          { children: [ diagramView {} ]
          }
      ]

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
    div "sidebar__action"
      [ tooltip name
          [ icon
              { name: iconName
              , className: "sidebar__action__icon"
              , onClick: handler preventDefault \_ -> store.dispatch (ToggleView view)
              }
          ]
      ]

-------------------------------------------------------------------------------
type IconProps
  = { className :: String
    , name :: String
    , onClick :: EventHandler
    }

icon :: IconProps -> JSX
icon props =
  SVG.svg
    { width: "16"
    , height: "16"
    , fill: "none"
    , stroke: "currentColor"
    , strokeWidth: "2"
    , strokeLinecap: "round"
    , strokeLinejoin: "round"
    , className: props.className
    , onClick: props.onClick
    , children:
        [ SVG.use
            { xlinkHref: featherIcons <> "#" <> props.name
            }
        ]
    }

-------------------------------------------------------------------------------
type DrawerProps
  = { title :: String
    , className :: String
    , content :: Array JSX
    }

drawer :: DrawerProps -> JSX
drawer { title, className, content } =
  div ("drawer " <> className)
    [ div "drawer__header"
        [ div "drawer__title" [ D.text title ]
        ]
    , div "drawer__content" content
    ]

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
  React.component "SignatureDrawer" \{ store } ->
    pure
      $ drawer
          { title: "Signature"
          , className: "signature"
          , content: []
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
          }

-------------------------------------------------------------------------------
makeDiagramView :: React.Component {}
makeDiagramView = do
  diagramComp <- makeDiagram
  pure \_ ->
    diagramComp
      { id: "diagram"
      , scale: { x: 50.0, y: 50.0 }
      , style2d:
          { pointRadius: 4.0
          , wireThickness: 3.0
          , crossingThickness: 6.0
          }
      , colors:
          Map.fromFoldable
            [ Generator { id: 0, dimension: 0 } /\ "lightgray"
            , Generator { id: 1, dimension: 1 } /\ "black"
            , Generator { id: 2, dimension: 2 } /\ "blue"
            , Generator { id: 3, dimension: 3 } /\ "red"
            ]
      , diagram: Diagram.source associativity
      }

associativity :: DiagramN
associativity =
  let
    attach b e s l = unsafePartial $ fromJust $ Diagram.attach b e s l

    x = Generator { id: 0, dimension: 0 }

    f = Generator { id: 1, dimension: 1 }

    m = Generator { id: 2, dimension: 2 }

    a = Generator { id: 3, dimension: 3 }

    fd = Diagram.fromGenerator (Diagram0 x) (Diagram0 x) f

    ffd = attach Target Nil fd fd

    md = Diagram.fromGenerator (DiagramN ffd) (DiagramN fd) m

    mfd = attach Target Nil fd md

    ld = attach Target Nil md mfd

    fmd = attach Source Nil fd md

    rd = attach Target Nil md fmd

    ad = Diagram.fromGenerator (DiagramN ld) (DiagramN rd) a
  in
    ad
