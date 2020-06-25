module Homotopy.Core.Generator (Generator, make, id, dimension) where

import Prelude (class Eq, class Ord)

newtype Generator
  = Generator { id :: Int, dimension :: Int }

derive instance eqGenerator :: Eq Generator

derive instance ordGenerator :: Ord Generator

make :: Int -> Int -> Generator
make i d = Generator { id: i, dimension: d }

id :: Generator -> Int
id (Generator g) = g.id

dimension :: Generator -> Int
dimension (Generator g) = g.dimension
