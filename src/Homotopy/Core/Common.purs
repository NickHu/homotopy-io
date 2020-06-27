module Homotopy.Core.Common
  ( Generator(..)
  , Height(..)
  , SliceIndex(..)
  , Boundary(..)
  ) where

import Prelude (class Eq, class Ord)

newtype Generator
  = Generator { id :: Int, dimension :: Int }

derive instance eqGenerator :: Eq Generator

derive instance ordGenerator :: Ord Generator

data Height
  = Regular Int
  | Singular Int

derive instance eqHeight :: Eq Height

data SliceIndex
  = Interior Height
  | Boundary Boundary

derive instance eqSliceIndex :: Eq SliceIndex

data Boundary
  = Source
  | Target

derive instance eqBoundary :: Eq Boundary
