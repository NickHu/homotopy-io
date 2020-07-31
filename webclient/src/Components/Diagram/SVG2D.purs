module Homotopy.Webclient.Components.Diagram.SVG2D (Props, Style, svg2d) where

import Homotopy.Webclient.Blocks
import Prelude
import Data.Array as Array
import Data.Foldable (fold, foldMap, maximum, minimum)
import Data.Function (on)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid (guard)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Homotopy.Core.Common (Boundary(..), Generator, Height(..), SliceIndex(..))
import Homotopy.Core.Diagram (Diagram)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Layout (Layout, layoutPosition)
import Homotopy.Core.Projection as Projection
import Math (abs)
import Partial.Unsafe (unsafePartial)
import React.Basic (JSX)
import React.Basic.DOM.SVG as SVG

type Props
  = { colors :: Map Generator String
    , scale :: { x :: Number, y :: Number }
    , id :: String
    , style :: Style
    , diagram :: Diagram
    , layout :: Layout
    }

type Style
  = { pointRadius :: Number
    , wireThickness :: Number
    , crossingThickness :: Number
    }

type PointMeta
  = { position :: Number
    , generator :: Generator
    }

svg2d :: Partial => Props -> JSX
svg2d ctx =
  SVG.svg
    { width: show width
    , height: show height
    , viewBox
    , children
    }
  where
  blockElement = case _ of
    BlockIdentity block -> identityElement ctx block
    BlockCell block -> cellElement ctx block

  children =
    map blockElement
      $ fromJust
      $ (traverse <<< traverse) annotatePoint
      $ blocks ctx.diagram

  width = (fromJust (layoutPosition ctx.layout (Boundary Target) (Boundary Target)) - 1.0) * ctx.scale.x

  height = toNumber (Diagram.size (Diagram.toDiagramN ctx.diagram)) * 2.0 * ctx.scale.y

  -- The coordinate system of SVG starts on the top border, while diagrams
  -- are layed out from the bottom. We accommodate for that by inverting the
  -- vertical coordinates and then moving the view box.
  viewBox = "0 " <> show (-height) <> " " <> show width <> " " <> show height

  -- Obtain the layout X coordinate and the generator at a point in the diagram.
  annotatePoint { x, y } = do
    position <- map (_ * ctx.scale.x) $ layoutPosition ctx.layout (Interior y) (Interior x)
    generator <- Projection.generatorAt ctx.diagram (Interior y : Interior x : Nil)
    pure { position, generator }

heightToNumber :: Height -> Number
heightToNumber = case _ of
  Regular i -> -toNumber (i * 2)
  Singular i -> -toNumber (i * 2 + 1)

unsafeGetColor :: Map Generator String -> Generator -> String
unsafeGetColor colors generator = unsafePartial $ fromJust $ Map.lookup generator colors

-------------------------------------------------------------------------------
-- | SVG element for an identity block.
identityElement :: Props -> BlockIdentity PointMeta -> JSX
identityElement ctx { source, target, center, row, index, height } = fold [ left, right, wire ]
  where
  heights =
    { source: heightToNumber (Regular row) * ctx.scale.y
    , target: heightToNumber (Regular (row + height)) * ctx.scale.y
    }

  left =
    SVG.rect
      { width: show $ center.position - source.position
      , x: show source.position
      , y: show $ min heights.source heights.target
      , height: show $ abs $ heights.target - heights.source
      , fill: unsafeGetColor ctx.colors source.generator
      , stroke: unsafeGetColor ctx.colors source.generator
      , strokeWidth: "1"
      }

  right =
    SVG.rect
      { width: show $ target.position - center.position
      , x: show center.position
      , y: show $ min heights.source heights.target
      , height: show $ abs $ heights.target - heights.source
      , fill: unsafeGetColor ctx.colors target.generator
      , stroke: unsafeGetColor ctx.colors source.generator
      , strokeWidth: "1"
      }

  wire =
    SVG.path
      { d: "M " <> show center.position <> " " <> show heights.source <> " L " <> show center.position <> " " <> show heights.target
      , strokeWidth: show ctx.style.wireThickness
      , stroke: unsafeGetColor ctx.colors center.generator
      }

-------------------------------------------------------------------------------
-- | SVG element for a cell block.
cellElement :: Props -> BlockCell PointMeta -> JSX
cellElement ctx { source, target, center, row, index } = SVG.g { children }
  where
  children =
    [ makeMask
    , fold $ makeSurfaces Source (Array.toUnfoldable source)
    , fold $ makeSurfaces Target (Array.toUnfoldable target)
    , fold $ map (makeWire Source) $ Array.sortBy (compare `on` _.depth) source
    , fold $ map (makeWire Target) $ Array.sortBy (compare `on` _.depth) target
    , makePoint
    ]

  boundaryHeight :: Boundary -> Number
  boundaryHeight = case _ of
    Source -> heightToNumber (Regular row) * ctx.scale.y
    Target -> heightToNumber (Regular (row + 1)) * ctx.scale.y

  middleHeight :: Number
  middleHeight = heightToNumber (Singular row) * ctx.scale.y

  wireCurve :: Boundary -> Number -> Number -> Cubic
  wireCurve boundary sx tx =
    let
      s = { x: sx, y: boundaryHeight boundary }

      t = { x: tx, y: middleHeight }

      sc = { x: s.x, y: (4.0 * s.y + t.y) / 5.0 }

      tc = { x: s.x, y: t.y }
    in
      Cubic s sc tc t

  -----------------------------------------------------------------------------
  -- Point
  -----------------------------------------------------------------------------
  makePoint :: JSX
  makePoint =
    SVG.circle
      { cx: show center.center.position
      , cy: show middleHeight
      , fill: unsafeGetColor ctx.colors center.center.generator
      , r: show ctx.style.pointRadius
      }

  -----------------------------------------------------------------------------
  -- Wires
  --
  -- For each input/output of the rendered 2-cell there is a wire connecting
  -- the source/target boundary to the center point of the cell diagram. The
  -- curve of the wire is calculated based on the horizontal displacement of
  -- the wire's start point relative to the center point.
  -----------------------------------------------------------------------------
  makeWire :: Boundary -> BoundaryCell PointMeta -> JSX
  makeWire boundary w =
    SVG.path
      { stroke: unsafeGetColor ctx.colors w.center.generator
      , strokeWidth: show ctx.style.wireThickness
      , d: "M" <> cubicPath (wireCurve boundary w.center.position center.center.position)
      , fill: "none"
      , strokeLinecap: "round"
      , mask: if Just w.depth == frontDepth then "" else "url(#" <> maskId <> ")"
      }

  -----------------------------------------------------------------------------
  -- Surfaces
  --
  -- The surfaces of the cell are created independently for the source and the
  -- target boundary. There is a surface between any two adjacent wires as
  -- well as the left and rightmost border of the cell diagram. The left and
  -- right border of the surfaces follow the same curve as the wires that they
  -- connect.
  -----------------------------------------------------------------------------
  makeSurfaces :: Boundary -> List (BoundaryCell PointMeta) -> List JSX
  makeSurfaces boundary wires =
    let
      leftBorder = center.source.position /\ center.source.position

      rightBorder = center.target.position /\ center.target.position
    in
      map (\(generator /\ left /\ right) -> makeSurface boundary generator left right)
        $ List.zip (center.source.generator : map _.target.generator wires)
        $ pairsPadded leftBorder rightBorder
        $ map (\wire -> wire.center.position /\ center.center.position) wires

  makeSurface :: Boundary -> Generator -> Number /\ Number -> Number /\ Number -> JSX
  makeSurface boundary generator (a0 /\ a1) (b0 /\ b1) =
    SVG.path
      { d:
          fold
            [ "M" <> cubicPath (cubicRev (wireCurve boundary a0 a1))
            , "L" <> cubicPath (wireCurve boundary b0 b1)
            , "Z"
            ]
      , fill: unsafeGetColor ctx.colors generator
      , stroke: unsafeGetColor ctx.colors generator
      , strokeWidth: "1"
      }

  -----------------------------------------------------------------------------
  -- Mask
  --
  -- When the wires have multiple depths, we create a mask that contains 
  -- thickened up versions of the wires in the front. All other wires behind
  -- are then hidden in the vicinity of the front wires to give the impression
  -- of a crossing.
  -----------------------------------------------------------------------------
  makeMask :: JSX
  makeMask =
    guard (frontDepth /= backDepth)
      $ SVG.defs
          { children:
              [ SVG.mask
                  { id: maskId
                  , maskUnits: "userSpaceOnUse"
                  , children:
                      [ SVG.rect { width: "100%", height: "100%", fill: "white" }
                      , foldMap (makeMaskWire Source) $ Array.filter (\c -> Just c.depth == frontDepth) source
                      , foldMap (makeMaskWire Target) $ Array.filter (\c -> Just c.depth == frontDepth) target
                      ]
                  }
              ]
          }

  makeMaskWire :: Boundary -> BoundaryCell PointMeta -> JSX
  makeMaskWire boundary w =
    SVG.path
      { stroke: "black"
      , strokeWidth: show ctx.style.crossingThickness
      , d: "M" <> cubicPath (wireCurve boundary w.center.position center.center.position)
      , fill: "none"
      , strokeLinecap: "round"
      }

  frontDepth :: Maybe Int
  frontDepth = minimum $ map _.depth (source <> target)

  backDepth :: Maybe Int
  backDepth = maximum $ map _.depth (source <> target)

  maskId :: String
  maskId = ctx.id <> "-mask-" <> show row <> "-" <> show index

-------------------------------------------------------------------------------
-- | All consecutive pairs of elements in a list, where the beginning and the
-- | end of the list are padded.
-- |
-- | Examples:
-- |  - `pairsPadded 0 3 (1 : 2 : Nil) = Tuple 0 1 : Tuple 1 2 : Tuple 2 3 : Nil`
-- |  - `pairsPadded 0 1 Nil = Tuple 0 1 : Nil`
pairsPadded :: forall a. a -> a -> List a -> List (Tuple a a)
pairsPadded start stop xs = go start xs
  where
  go x = case _ of
    y : ys -> Tuple x y : go y ys
    Nil -> Tuple x stop : Nil

arrayInit :: forall a. Int -> (Int -> a) -> Array a
arrayInit n f = if n == 0 then [] else map f (Array.range 0 (n - 1))

data Cubic
  = Cubic (Point2D Number) (Point2D Number) (Point2D Number) (Point2D Number)

cubicPath :: Cubic -> String
cubicPath (Cubic s sc tc t) = svgPoint s <> "C" <> svgPoint sc <> "," <> svgPoint tc <> "," <> svgPoint t

cubicRev :: Cubic -> Cubic
cubicRev (Cubic s sc tc t) = Cubic t tc sc s

svgPoint :: Point2D Number -> String
svgPoint { x, y } = show x <> " " <> show y
