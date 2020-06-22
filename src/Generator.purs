module Generator (Generator, make, id, dimension) where

type Generator
  = { id :: Int
    , dimension :: Int
    }

make :: Int -> Int -> Generator
make i d = { id: i, dimension: d }

id :: Generator -> Int
id g = g.id

dimension :: Generator -> Int
dimension g = g.dimension
