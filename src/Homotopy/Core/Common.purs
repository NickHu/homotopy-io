module Homotopy.Core.Common
  ( Generator(..)
  , Height(..)
  , SliceIndex(..)
  , Boundary(..)
  ) where

import Data.Tuple (Tuple(..))
import Prelude (class Bounded, class Eq, class Ord, compare, (*), (+))

newtype Generator
  = Generator { id :: Int, dimension :: Int }

derive instance eqGenerator :: Eq Generator

derive instance ordGenerator :: Ord Generator

data Height
  = Regular Int
  | Singular Int

derive instance eqHeight :: Eq Height

instance ordHeight :: Ord Height where
  compare a b = compare (toInt a) (toInt b)
    where
    toInt (Regular i) = i * 2

    toInt (Singular i) = i * 2 + 1

data SliceIndex
  = Interior Height
  | Boundary Boundary

instance ordSliceIndex :: Ord SliceIndex where
  compare x y = compare (toTuple x) (toTuple y)
    where
    toTuple (Boundary Source) = Tuple 0 0

    toTuple (Interior (Regular i)) = Tuple 1 (i * 2)

    toTuple (Interior (Singular i)) = Tuple 1 (i * 2 + 1)

    toTuple (Boundary Target) = Tuple 2 0

instance boundedSliceIndex :: Bounded SliceIndex where
  bottom = Boundary Source
  top = Boundary Target

derive instance eqSliceIndex :: Eq SliceIndex

data Boundary
  = Source
  | Target

derive instance eqBoundary :: Eq Boundary
