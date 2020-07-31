module Homotopy.Webclient.Components.Transition where

import Prelude
import Data.Array as Array
import Data.Foldable (fold, or)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (liftEffect)
import React.Basic (JSX, ReactComponent, element, fragment)
import React.Basic.DOM as D
import React.Basic.DOM.SVG as SVG
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

type TransitionClasses
  = { appear :: String
    , appearActive :: String
    , appearDone :: String
    , enter :: String
    , enterActive :: String
    , enterDone :: String
    , exit :: String
    , exitActive :: String
    , exitDone :: String
    }

type Timeout
  = { appear :: Number
    , enter :: Number
    , exit :: Number
    }

type CssTransitionProps
  = { in :: Boolean
    , classNames :: TransitionClasses
    , unmountOnExit :: Boolean
    , mountOnEnter :: Boolean
    , children :: JSX
    , timeout :: Timeout
    }

foreign import cssTransitionComponent ::
  ReactComponent CssTransitionProps

transitionClasses :: String -> TransitionClasses
transitionClasses base =
  { appear: base <> "--appear"
  , appearActive: base <> "--appear-active"
  , appearDone: base <> "--appear-done"
  , enter: base <> "--enter"
  , enterActive: base <> "--enter-active"
  , enterDone: base <> "--enter-done"
  , exit: base <> "--exit"
  , exitActive: base <> "--exit-active"
  , exitDone: base <> "--exit-done"
  }

cssTransition :: CssTransitionProps -> JSX
cssTransition = element cssTransitionComponent

type SwitchProps
  = { className :: String
    , timeout :: Number
    , children :: Maybe JSX
    }

makeSwitch :: Component SwitchProps
makeSwitch =
  component "Switch" \props -> React.do
    lastRef <- React.useRef Nothing
    last <-
      React.unsafeRenderEffect do
        last <- React.readRef lastRef
        React.writeRef lastRef props.children
        pure last
    let
      child = case props.children, last of
        Just e, _ -> Just e
        Nothing, Just e -> Just e
        Nothing, Nothing -> Nothing
    pure
      $ cssTransition
          { classNames: transitionClasses props.className
          , timeout: { enter: props.timeout, exit: props.timeout, appear: props.timeout }
          , unmountOnExit: true
          , mountOnEnter: true
          , children: fromMaybe (fragment []) child
          , in: isJust props.children
          }
