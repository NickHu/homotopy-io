module Homotopy.Core.Common
  ( Boundary(..)
  , Generator(..)
  , Height(..)
  , SliceIndex(..)
  ) where

import Data.Enum (class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Prelude (class Bounded, class Eq, class Ord, class Show, compare, show, (*), (+), (-), (<>))

newtype Generator
  = Generator { id :: Int, dimension :: Int }

derive instance newtypeGenerator :: Newtype Generator _

derive newtype instance eqGenerator :: Eq Generator

derive newtype instance ordGenerator :: Ord Generator

derive instance genericGenerator :: Generic Generator _

derive newtype instance hashableGenerator :: Hashable Generator

instance showGenerator :: Show Generator where
  show (Generator g) = show g.id <> "!" <> show g.dimension

data Height
  = Regular Int
  | Singular Int

derive instance eqHeight :: Eq Height

derive instance genericHeight :: Generic Height _

instance ordHeight :: Ord Height where
  compare a b = compare (toInt a) (toInt b)
    where
    toInt (Regular i) = i * 2

    toInt (Singular i) = i * 2 + 1

instance enumHeight :: Enum Height where
  succ (Regular i) = Just (Singular i)
  succ (Singular i) = Just (Regular (i + 1))
  pred (Regular i) = Just (Singular (i - 1))
  pred (Singular i) = Just (Regular i)

instance showHeight :: Show Height where
  show (Regular i) = "R" <> show i
  show (Singular i) = "S" <> show i

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

derive instance genericSliceIndex :: Generic SliceIndex _

instance showSliceIndex :: Show SliceIndex where
  show x = genericShow x

data Boundary
  = Source
  | Target

derive instance eqBoundary :: Eq Boundary

derive instance genericBoundary :: Generic Boundary _

instance showBoundary :: Show Boundary where
  show x = genericShow x
