module Homotopy.Webclient.Component.Transition (cssTransition, Props, Timeout, option) where

import Prelude
import Concur.Core (Widget, mkLeafWidget, mkNodeWidget)
import Concur.React (HTML)
import Data.Maybe (Maybe(..))
import Effect.Ref as Ref
import React as React

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

foreign import cssTransitionClass ::
  React.ReactClass
    { in :: Boolean
    , timeout :: Timeout
    , classNames :: TransitionClasses
    , unmountOnExit :: Boolean
    , mountOnEnter :: Boolean
    , children :: React.ReactElement
    }

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

type Props
  = { in :: Boolean
    , timeout :: Timeout
    , className :: String
    , unmountOnExit :: Boolean
    , mountOnEnter :: Boolean
    }

cssTransition :: forall a. Props -> Widget HTML a -> Widget HTML a
cssTransition props =
  mkNodeWidget \_ ->
    map \element ->
      React.createLeafElement cssTransitionClass
        { in: props.in
        , timeout: props.timeout
        , classNames: transitionClasses props.className
        , unmountOnExit: props.unmountOnExit
        , mountOnEnter: props.mountOnEnter
        , children: element
        }

-------------------------------------------------------------------------------
optionComponent :: React.ReactClass { children :: Maybe (React.ReactElement), timeout :: Timeout, className :: String }
optionComponent =
  React.component "Option" \this -> do
    lastRef <- Ref.new Nothing
    pure
      { render:
          do
            props <- React.getProps this
            last <- Ref.read lastRef
            case last, props.children of
              _, Just element -> do
                Ref.write (Just element) lastRef
                pure
                  $ React.createLeafElement cssTransitionClass
                      { in: true
                      , timeout: props.timeout
                      , classNames: transitionClasses props.className
                      , unmountOnExit: true
                      , mountOnEnter: true
                      , children: element
                      }
              Just element, Nothing ->
                pure
                  $ React.createLeafElement cssTransitionClass
                      { in: false
                      , timeout: props.timeout
                      , classNames: transitionClasses props.className
                      , unmountOnExit: true
                      , mountOnEnter: true
                      , children: element
                      }
              Nothing, Nothing ->
                pure
                  $ React.createLeafElement cssTransitionClass
                      { in: false
                      , timeout: props.timeout
                      , classNames: transitionClasses props.className
                      , unmountOnExit: true
                      , mountOnEnter: true
                      , children: (mempty :: React.ReactElement)
                      }
      }

option :: forall a. { timeout :: Number, className :: String } -> Maybe (Widget HTML a) -> Widget HTML a
option props = case _ of
  Just widget ->
    mkNodeWidget
      ( \_ ->
          map \element ->
            React.createLeafElement optionComponent
              { timeout: { appear: 0.0, enter: props.timeout, exit: props.timeout }
              , className: props.className
              , children: Just element
              }
      )
      widget
  Nothing ->
    mkLeafWidget \_ ->
      [ React.createLeafElement optionComponent
          { timeout: { appear: 0.0, enter: props.timeout, exit: props.timeout }
          , className: props.className
          , children: Nothing
          }
      ]
