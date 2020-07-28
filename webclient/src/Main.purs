module Homotopy.Webclient.Main where

import Prelude
import Concur.Core (Widget)
import Concur.Core.Patterns (tea)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Homotopy.Webclient.Component.Icon as Icon
import Homotopy.Webclient.Component.Transition (option)
import Homotopy.Webclient.State (State, Action(..))
import Homotopy.Webclient.State as State

foreign import logoSVG :: String

main :: Effect Unit
main = runWidgetInDom "app" (tea State.initial root State.reduce)
  where
  root state =
    fold
      [ sidebar state
      , drawer state
      , div "workspace" []
      ]

div :: forall a. String -> Array (Widget HTML a) -> Widget HTML a
div className children = D.div [ P.className className ] children

sidebar :: State -> Widget HTML Action
sidebar state =
  div "sidebar"
    [ div "sidebar__logo" [ logo ]
    , div "sidebar__actions"
        [ toggle { view: State.ViewProject, icon: "book", name: "Project" }
        , toggle { view: State.ViewSignature, icon: "box", name: "Signature" }
        , toggle { view: State.ViewUser, icon: "user", name: "User" }
        ]
    ]
  where
  logo =
    D.img
      [ P.src logoSVG
      , P.className "sidebar__logo__image"
      , P.alt "homotopy.io"
      , P.title "homotopy.io"
      ]

  toggle { name, icon, view } =
    let
      active = if state.view == Just view then "sidebar__action--active" else ""
    in
      D.div [ P.className $ "sidebar__action " <> active ]
        [ D.div [ P.className "tooltip tooltip--right", P.unsafeMkProp "data-tooltip" name ]
            [ Icon.icon icon
                [ P.className "sidebar__action__icon"
                , P.title name
                , ToggleView view <$ P.onClick
                ]
            ]
        ]

drawer :: State -> Widget HTML Action
drawer state = transition (map showDrawer state.view)
  where
  transition = option { timeout: 150.0, className: "drawer" }

  showDrawer = case _ of
    State.ViewSignature -> signatureDrawer state
    State.ViewUser -> userDrawer state
    State.ViewProject -> projectDrawer state

type DrawerProps a
  = { className :: String
    , title :: String
    , content :: Widget HTML a
    }

makeDrawer :: forall a. DrawerProps a -> Widget HTML a
makeDrawer props =
  div ("drawer " <> props.className)
    [ div "drawer__header"
        [ div "drawer__title" [ D.text props.title ]
        ]
    , div "drawer__content" [ props.content ]
    ]

projectDrawer :: State -> Widget HTML Action
projectDrawer state = makeDrawer { title: "Project", className: "project", content }
  where
  content = mempty

signatureDrawer :: State -> Widget HTML Action
signatureDrawer state = makeDrawer { title: "Signature", className: "signature", content }
  where
  content = mempty

userDrawer :: State -> Widget HTML Action
userDrawer state = makeDrawer { title: "User", className: "user", content }
  where
  content = mempty
