module Homotopy.Webclient.Diagram2D (Context, Style, diagramSVG) where

import Homotopy.Webclient.Blocks
import Prelude
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.Props as P
import Concur.React.SVG as SVG
import Data.Array as Array
import Data.Foldable (fold, foldMap, maximum, minimum)
import Data.Function (on)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid (guard)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Homotopy.Core.Common (Boundary(..), Generator, Height(..), SliceIndex(..))
import Homotopy.Core.Diagram (Diagram)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Layout (Layout, layoutPosition)
import Math (abs)
import Homotopy.Core.Projection as Projection

type Context
  = { colors :: Generator -> String
    , scale :: { x :: Number, y :: Number }
    , id :: String
    , style :: Style
    }

type Style
  = { pointRadius :: Number
    , wireThickness :: Number
    , crossingThickness :: Number
    }

type Event
  = Point2D Int

type Point
  = { position :: Number
    , generator :: Generator
    }

diagramSVG :: Partial => Context -> Layout -> Diagram -> Widget HTML { x :: Int, y :: Int }
diagramSVG ctx layout diagram =
  SVG.svg props
    $ map blockElement
    $ fromJust
    $ (traverse <<< traverse) annotatePoint
    $ blocks diagram
  where
  blockElement = case _ of
    BlockIdentity block -> identityElement ctx block
    BlockCell block -> cellElement ctx block

  -- The coordinate system of SVG starts on the top border, while diagrams
  -- are layed out from the bottom. We accommodate for that by inverting the
  -- vertical coordinates and then moving the view box.
  props =
    [ P.width (show width)
    , P.height (show height)
    , P.viewBox $ "0 " <> show (-height) <> " " <> show width <> " " <> show height
    ]

  width = (fromJust (layoutPosition layout (Boundary Target) (Boundary Target)) - 1.0) * ctx.scale.x

  height = toNumber (Diagram.size (Diagram.toDiagramN diagram)) * 2.0 * ctx.scale.y

  -- Obtain the layout X coordinate and the generator at a point in the diagram.
  annotatePoint { x, y } = do
    position <- map (_ * ctx.scale.x) $ layoutPosition layout (Interior y) (Interior x)
    generator <- Projection.generatorAt diagram (Interior y : Interior x : Nil)
    pure { position, generator }

heightToNumber :: Height -> Number
heightToNumber = case _ of
  Regular i -> -toNumber (i * 2)
  Singular i -> -toNumber (i * 2 + 1)

-------------------------------------------------------------------------------
-- | SVG element for an identity block.
identityElement :: Context -> BlockIdentity Point -> Widget HTML Event
identityElement ctx { source, target, center, row, index, height } = fold [ left, right, wire ]
  where
  heights =
    { source: heightToNumber (Regular row) * ctx.scale.y
    , target: heightToNumber (Regular (row + height)) * ctx.scale.y
    }

  left =
    SVG.rect
      [ P.width $ show $ center.position - source.position
      , P.unsafeMkProp "x" $ show source.position
      , P.unsafeMkProp "y" $ show $ min heights.source heights.target
      , P.height $ show $ abs $ heights.target - heights.source
      , P.fill (ctx.colors source.generator)
      , P.stroke (ctx.colors source.generator)
      , P.strokeWidth 1
      ]
      []

  right =
    SVG.rect
      [ P.width $ show $ target.position - center.position
      , P.unsafeMkProp "x" $ show center.position
      , P.unsafeMkProp "y" $ show $ min heights.source heights.target
      , P.height $ show $ abs $ heights.target - heights.source
      , P.fill (ctx.colors target.generator)
      , P.stroke (ctx.colors source.generator)
      , P.strokeWidth 1
      ]
      []

  wire =
    SVG.path
      [ P.d ("M " <> show center.position <> " " <> show heights.source <> " L " <> show center.position <> " " <> show heights.target)
      , P.unsafeMkProp "strokeWidth" ctx.style.wireThickness
      , P.stroke (ctx.colors center.generator)
      ]
      []

-------------------------------------------------------------------------------
-- | SVG element for a cell block.
cellElement :: Context -> BlockCell Point -> Widget HTML Event
cellElement ctx { source, target, center, row, index } =
  SVG.g []
    ( [ makeMask
      , fold $ makeSurfaces Source (Array.toUnfoldable source)
      , fold $ makeSurfaces Target (Array.toUnfoldable target)
      , fold $ map (makeWire Source) $ Array.sortBy (compare `on` _.depth) source
      , fold $ map (makeWire Target) $ Array.sortBy (compare `on` _.depth) target
      , makePoint
      ]
    )
  where
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

  onClickEvent = { y: row, x: index } <$ P.onClick

  -----------------------------------------------------------------------------
  -- Point
  -----------------------------------------------------------------------------
  makePoint :: Widget HTML Event
  makePoint =
    SVG.circle
      [ P.unsafeMkProp "cx" $ show center.center.position
      , P.unsafeMkProp "cy" $ show middleHeight
      , P.fill (ctx.colors center.center.generator)
      , P.unsafeMkProp "r" $ show ctx.style.pointRadius
      , onClickEvent
      ]
      []

  -----------------------------------------------------------------------------
  -- Wires
  --
  -- For each input/output of the rendered 2-cell there is a wire connecting
  -- the source/target boundary to the center point of the cell diagram. The
  -- curve of the wire is calculated based on the horizontal displacement of
  -- the wire's start point relative to the center point.
  -----------------------------------------------------------------------------
  makeWire :: Boundary -> BoundaryCell Point -> Widget HTML Event
  makeWire boundary w =
    SVG.path
      [ P.stroke (ctx.colors w.center.generator)
      , P.unsafeMkProp "strokeWidth" ctx.style.wireThickness
      , P.d $ "M" <> cubicPath (wireCurve boundary w.center.position center.center.position)
      , P.fill "none"
      , P.unsafeMkProp "strokeLinecap" "round"
      , onClickEvent
      , if Just w.depth == frontDepth then P.emptyProp else (P.unsafeMkProp "mask" $ "url(#" <> maskId <> ")")
      ]
      []

  -----------------------------------------------------------------------------
  -- Surfaces
  --
  -- The surfaces of the cell are created independently for the source and the
  -- target boundary. There is a surface between any two adjacent wires as
  -- well as the left and rightmost border of the cell diagram. The left and
  -- right border of the surfaces follow the same curve as the wires that they
  -- connect.
  -----------------------------------------------------------------------------
  makeSurfaces :: Boundary -> List (BoundaryCell Point) -> List (Widget HTML Event)
  makeSurfaces boundary wires =
    let
      leftBorder = center.source.position /\ center.source.position

      rightBorder = center.target.position /\ center.target.position
    in
      map (\(generator /\ left /\ right) -> makeSurface boundary generator left right)
        $ List.zip (center.source.generator : map _.target.generator wires)
        $ pairsPadded leftBorder rightBorder
        $ map (\wire -> wire.center.position /\ center.center.position) wires

  makeSurface :: Boundary -> Generator -> Number /\ Number -> Number /\ Number -> Widget HTML Event
  makeSurface boundary generator (a0 /\ a1) (b0 /\ b1) =
    SVG.path
      [ P.d
          $ fold
              [ "M" <> cubicPath (cubicRev (wireCurve boundary a0 a1))
              , "L" <> cubicPath (wireCurve boundary b0 b1)
              , "Z"
              ]
      , P.fill (ctx.colors generator)
      , P.stroke (ctx.colors generator)
      , P.strokeWidth 1
      , onClickEvent
      ]
      []

  -----------------------------------------------------------------------------
  -- Mask
  --
  -- When the wires have multiple depths, we create a mask that contains 
  -- thickened up versions of the wires in the front. All other wires behind
  -- are then hidden in the vicinity of the front wires to give the impression
  -- of a crossing.
  -----------------------------------------------------------------------------
  makeMask :: Widget HTML Event
  makeMask =
    guard (frontDepth /= backDepth)
      $ SVG.defs []
          [ SVG.mask
              [ P._id maskId
              , P.unsafeMkProp "maskUnits" "userSpaceOnUse"
              ]
              [ SVG.rect [ P.width "100%", P.height "100%", P.fill "white" ] []
              , foldMap (makeMaskWire Source) $ Array.filter (\c -> Just c.depth == frontDepth) source
              , foldMap (makeMaskWire Target) $ Array.filter (\c -> Just c.depth == frontDepth) target
              ]
          ]

  makeMaskWire :: Boundary -> BoundaryCell Point -> Widget HTML Event
  makeMaskWire boundary w =
    SVG.path
      [ P.stroke "black"
      , P.unsafeMkProp "strokeWidth" $ show ctx.style.crossingThickness
      , P.d $ "M" <> cubicPath (wireCurve boundary w.center.position center.center.position)
      , P.fill "none"
      , P.unsafeMkProp "stroke-linecap" "round"
      ]
      []

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
