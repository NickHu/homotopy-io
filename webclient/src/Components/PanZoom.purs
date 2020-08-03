module Homotopy.Webclient.Components.PanZoom
  ( PanZoomProps
  , makePanZoom
  ) where

import Prelude
import Data.Foldable (fold)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, null)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import Homotopy.Webclient.Components.PanZoom.State (State, Event(..), reduce, Pointer, V2, initialState)
import React.Basic.DOM as D
import React.Basic.DOM.Events (clientX, clientY, ctrlKey, nativeEvent, preventDefault)
import React.Basic.Events (EventFn, EventHandler, SyntheticEvent, handler, merge, unsafeEventFn)
import React.Basic.Hooks as React
import Web.DOM (Node)
import Web.Event.Event as WebEvent
import Web.Event.Internal.Types (EventTarget)
import Web.HTML.HTMLElement (DOMRect, fromNode, fromEventTarget, getBoundingClientRect)

-------------------------------------------------------------------------------
type PanZoomProps
  = { children :: Array React.JSX
    }

makePanZoom :: React.Component PanZoomProps
makePanZoom = do
  React.component "PanZoom" \props -> React.do
    { transform
    , onPointerDown
    , onPointerUp
    , onPointerMove
    , onPointerOut
    , onPointerLeave
    , onPointerCancel
    , onWheel
    , ref
    , debug
    } <-
      usePanZoom
    pure
      ( D.div
          { className: "panzoom"
          , onPointerDown
          , onPointerUp
          , onPointerMove
          , onPointerOut
          , onPointerLeave
          , onPointerCancel
          , onWheel
          , ref
          , children:
              [ D.div
                  { className: "panzoom__child"
                  , style: D.css { transform }
                  , children: props.children
                  }
              ]
          }
      )

type Result
  = { transform :: String
    , onPointerDown :: EventHandler
    , onPointerUp :: EventHandler
    , onPointerMove :: EventHandler
    , onPointerOut :: EventHandler
    , onPointerLeave :: EventHandler
    , onPointerCancel :: EventHandler
    , onWheel :: EventHandler
    , ref :: React.Ref (Nullable Node)
    , debug :: String
    }

type NodeRef
  = React.Ref (Nullable Node)

newtype UsePanZoom h
  = UsePanZoom ((React.UseRef (Nullable Node) (React.UseReducer State Event h)))

derive instance usePanZoomNewtype :: Newtype (UsePanZoom h) _

usePanZoom :: React.Hook UsePanZoom Result
usePanZoom = React.coerceHook hook
  where
  hook = React.do
    reducer <- React.unsafeRenderEffect (React.mkReducer reduce)
    state /\ dispatch <- React.useReducer initialState reducer
    ref <- React.useRef null
    pure
      { transform: transformFromState state
      , ref
      , onPointerDown: onPointerDown ref dispatch
      , onPointerUp: onPointerUp ref dispatch
      , onPointerMove: onPointerMove ref dispatch
      , onPointerOut: onPointerOut ref dispatch
      , onPointerLeave: onPointerOut ref dispatch
      , onPointerCancel: onPointerOut ref dispatch
      , onWheel: onWheel ref dispatch
      , debug: show (Map.size state.touch)
      }

  transformFromState :: State -> String
  transformFromState state =
    fold
      [ "translate(" <> show state.translate.x <> "px, " <> show state.translate.y <> "px) "
      , "scale(" <> show state.scale <> ")"
      ]

  onPointerDown ref dispatch =
    handler
      (merge { clientX, clientY, ctrlKey, pointerId, pointerType, preventDefault }) \event -> case event.pointerType, event.ctrlKey of
      "touch", _ -> do
        position <- relativePosition ref event
        dispatch $ TouchDown event.pointerId position
      "mouse", Just true -> do
        position <- relativePosition ref event
        dispatch $ MouseDown position
      _, _ -> pure unit

  onPointerOut ref dispatch =
    handler
      (merge { pointerId, pointerType, preventDefault }) \event -> case event.pointerType of
      "touch" -> dispatch $ TouchUp event.pointerId
      _ -> pure unit

  onPointerUp ref dispatch =
    handler
      (merge { pointerId, pointerType, preventDefault }) \event -> case event.pointerType of
      "touch" -> dispatch $ TouchUp event.pointerId
      "mouse" -> dispatch MouseUp
      _ -> pure unit

  onPointerMove ref dispatch =
    handler
      (merge { clientX, clientY, pointerId, pointerType }) \event -> case event.pointerType of
      "touch" -> do
        position <- relativePosition ref event
        dispatch $ TouchMove event.pointerId position
      "mouse" -> do
        position <- relativePosition ref event
        dispatch $ MouseMove position
      _ -> pure unit

  onWheel ref dispatch =
    handler
      (merge { deltaY, clientX, clientY, ctrlKey, preventDefault }) \event ->
      if event.ctrlKey == Just true then do
        position <- relativePosition ref event
        dispatch $ MouseWheel position event.deltaY
      else
        pure unit

  relativePosition :: forall e. _ -> { clientX :: Maybe Number, clientY :: Maybe Number | e } -> Effect V2
  relativePosition ref { clientX, clientY } = do
    node <- maybe (throw "panzoom event handler failed") pure =<< React.readRefMaybe ref
    element <- maybe (throw "panzoom event handler failed") pure (fromNode node)
    { left, top } <- getBoundingClientRect element
    pure { x: fromMaybe 0.0 clientX - left, y: fromMaybe 0.0 clientY - top }

pointerId :: EventFn SyntheticEvent Pointer
pointerId = nativeEvent >>> unsafeEventFn unsafePointerId

pointerType :: EventFn SyntheticEvent String
pointerType = nativeEvent >>> unsafeEventFn unsafePointerType

deltaY :: EventFn SyntheticEvent Number
deltaY = nativeEvent >>> unsafeEventFn unsafeDeltaY

foreign import unsafePointerId :: WebEvent.Event -> Pointer

foreign import unsafePointerType :: WebEvent.Event -> String

foreign import unsafeDeltaY :: WebEvent.Event -> Number
