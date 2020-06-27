module Homotopy.Core.Common
  ( Generator(..)
  , Height(..)
  , SliceIndex(..)
  , Boundary(..)
  ) where

import Prelude (class Eq, class Ord, compare, (*), (+))

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

derive instance eqSliceIndex :: Eq SliceIndex

data Boundary
  = Source
  | Target

derive instance eqBoundary :: Eq Boundary
