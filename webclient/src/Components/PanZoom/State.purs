module Homotopy.Webclient.Components.PanZoom.State where

import Prelude
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.List ((:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Math (sqrt)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

data Event
  = TouchDown Pointer V2
  | TouchMove Pointer V2
  | TouchUp Pointer
  | MouseWheel V2 Number
  | MouseDown V2
  | MouseMove V2
  | MouseUp

type State
  = { translate :: V2
    , touch :: Map Pointer V2
    , mouse :: Maybe V2
    , scale :: Number
    }

initialState :: State
initialState =
  { translate: { x: 0.0, y: 0.0 }
  , touch: mempty
  , mouse: Nothing
  , scale: 1.0
  }

reduce :: State -> Event -> State
reduce state = case _ of
  -----------------------------------------------------------------------------
  TouchDown pointer position -> state { touch = Map.insert pointer position state.touch }
  -----------------------------------------------------------------------------
  TouchMove pointer position
    | Map.size state.touch /= 2 -> state { touch = Map.insert pointer position state.touch }
    | otherwise ->
      let
        touch = Map.insert pointer position state.touch

        touchDistance ts = case Map.values ts of
          p : q : _ -> max (euclideanDistance p q) 0.01
          _ -> unsafePartial (crashWith "Invariant failed: there need to be two touch points.")

        scale = state.scale * (touchDistance touch / touchDistance state.touch)

        previous = fromMaybe position $ Map.lookup pointer state.touch

        average = scaleV2 0.5 (sum touch)

        zoomPoint = scaleV2 (1.0 / state.scale) (average - state.translate)

        translate = state.translate + scaleV2 (state.scale - scale) zoomPoint + scaleV2 0.5 (position - previous)
      in
        state { touch = touch, translate = translate, scale = scale }
  -----------------------------------------------------------------------------
  TouchUp pointer -> state { touch = Map.delete pointer state.touch }
  -----------------------------------------------------------------------------
  MouseDown p -> state { mouse = Just p }
  -----------------------------------------------------------------------------
  MouseMove p -> case state.mouse of
    Just prev -> state { translate = state.translate + p - prev, mouse = Just p }
    Nothing -> state
  -----------------------------------------------------------------------------
  MouseUp -> state { mouse = Nothing }
  -----------------------------------------------------------------------------
  MouseWheel mouse z ->
    let
      scale = state.scale * (if z < 0.0 then 1.1 else 0.9)

      zoomPoint = scaleV2 (1.0 / state.scale) (mouse - state.translate)

      translate = state.translate + scaleV2 (state.scale - scale) zoomPoint
    in
      state { scale = scale, translate = translate }

type V2
  = { x :: Number, y :: Number }

newtype Pointer
  = Pointer Int

derive newtype instance pointerEq :: Eq Pointer

derive newtype instance pointerOrd :: Ord Pointer

derive newtype instance pointerShow :: Show Pointer

euclideanDistance :: V2 -> V2 -> Number
euclideanDistance p q =
  let
    d = p - q
  in
    sqrt ((d.x * d.x) + (d.y * d.y))

scaleV2 :: Number -> V2 -> V2
scaleV2 s { x, y } = { x: s * x, y: s * y }
