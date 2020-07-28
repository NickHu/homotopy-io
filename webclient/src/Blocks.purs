module Homotopy.Webclient.Blocks
  ( Block(..)
  , BlockIdentity
  , BlockCell
  , BoundaryCell
  , SingularCell
  , Point2D
  , blocks
  ) where

import Prelude
import Data.Array as Array
import Data.Const (Const(..))
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Homotopy.Core.Common (Height(..), SliceIndex(..))
import Homotopy.Core.Diagram (Diagram(..))
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Interval as Interval
import Homotopy.Core.Projection as Projection
import Homotopy.Core.Rewrite as Rewrite
import Record (union)

data Block a
  = BlockCell (BlockCell a)
  | BlockIdentity (BlockIdentity a)

type BlockIdentity a
  = { source :: a
    , target :: a
    , center :: a
    , index :: Int
    , row :: Int
    , height :: Int
    }

type BlockCell a
  = { source :: Array (BoundaryCell a)
    , target :: Array (BoundaryCell a)
    , center :: SingularCell a
    , row :: Int
    , index :: Int
    }

type BoundaryCell a
  = { source :: a
    , target :: a
    , center :: a
    , depth :: Int
    }

type SingularCell a
  = { source :: a
    , target :: a
    , center :: a
    }

type Point2D a
  = { x :: a
    , y :: a
    }

traverseBlock :: forall a b f. Applicative f => (a -> f b) -> Block a -> f (Block b)
traverseBlock go = case _ of
  BlockCell block -> BlockCell <$> traverseBlockCell go block
  BlockIdentity block -> BlockIdentity <$> traverseBlockIdentity go block
  where
  traverseBlockCell :: (a -> f b) -> BlockCell a -> f (BlockCell b)
  traverseBlockCell f block@{ index, row } = ado
    source <- (traverse <<< traverseBoundaryCell) f block.source
    target <- (traverse <<< traverseBoundaryCell) f block.target
    center <- traverseCenterCell f block.center
    in { row, index, source, target, center }

  traverseCenterCell f cell = ado
    source <- f cell.source
    center <- f cell.center
    target <- f cell.target
    in { source, center, target }

  traverseBoundaryCell f cell@{ depth } = ado
    source <- f cell.source
    center <- f cell.center
    target <- f cell.target
    in { source, center, target, depth }

  traverseBlockIdentity f block@{ row, index, height } = ado
    source <- f block.source
    center <- f block.center
    target <- f block.target
    in { source, center, target, row, index, height }

instance blockFunctor :: Functor Block where
  map f = unwrap <<< traverseBlock (Identity <<< f)

instance blockFoldable :: Foldable Block where
  foldMap f = unwrap <<< traverseBlock (Const <<< f)
  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance blockTraversable :: Traversable Block where
  traverse = traverseBlock
  sequence x = sequenceDefault x

blocks :: Partial => Diagram -> Array (Block (Point2D Height))
blocks d = map (blockAt d) (singularPoints d)

blockAt :: Partial => Diagram -> Point2D Int -> Block (Point2D Height)
blockAt d@(DiagramN diagram) { x, y } =
  if isCell then
    BlockCell
      { row: y
      , index: x
      , center: centerCell
      , source: boundary sourceSlice forward (Regular y)
      , target: boundary targetSlice backward (Regular (y + 1))
      }
  else
    BlockIdentity
      ( union centerCell
          { row: y
          , index: x
          , height: 1
          }
      )
  where
  Just { forward, backward } = Diagram.cospans diagram List.!! y

  sourceSlice = fromJust $ Diagram.sliceAt diagram (Interior (Regular y))

  targetSlice = fromJust $ Diagram.sliceAt diagram (Interior (Regular (y + 1)))

  isCell = List.elem x (Rewrite.targets forward) || List.elem x (Rewrite.targets backward)

  cellAt x_ y_ =
    { source: { x: Regular x_, y: y_ }
    , center: { x: Singular x_, y: y_ }
    , target: { x: Regular (x_ + 1), y: y_ }
    }

  centerCell = cellAt x (Singular y)

  boundary slice rewrite y_ =
    let
      depthAt i =
        if Diagram.dimension slice <= 2 then
          0
        else
          fromMaybe 0 $ map (Rewrite.singularImage (Rewrite.slice rewrite i)) $ Projection.pointDepth slice i
    in
      map (\x_ -> union { depth: depthAt x_ } (cellAt x_ y_))
        $ Interval.toUnfoldable
        $ Rewrite.singularPreimage rewrite x

singularPoints :: Partial => Diagram -> Array (Point2D Int)
singularPoints (DiagramN diagram) =
  join
    $ mapWithIndex (\y slice -> arrayInit (Diagram.size $ Diagram.toDiagramN slice) { y, x: _ })
    $ List.toUnfoldable
    $ Diagram.singularSlices diagram

arrayInit :: forall a. Int -> (Int -> a) -> Array a
arrayInit n f = if n == 0 then [] else map f (Array.range 0 (n - 1))
