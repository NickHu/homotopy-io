module Diagram
  ( Diagram
  , Slices
  , Boundary
  , identity
  , dimension
  , size
  , cospans
  , toGenerator
  , fromGenerator
  , source
  , target
  , slices
  , sliceAt
  , singularSlices
  , regularSlices
  , internalizeHeight
  ) where

import Data.Foldable (length, maximum)
import Data.List (List(..), concatMap, drop, last, scanl, tail, take, (!!), (:))
import Data.Maybe (Maybe(..), fromJust)
import Prelude (class Eq, map, otherwise, ($), (*), (+), (-), (<), (<>), (==), (>), (>=))
import Limit (Cone, Cospan, Height(..), Limit(..), SliceIndex(..), coneSize)
import Generator (Generator)

-- | A diagram is either $0$-dimensional, in which case it consists of a
-- | generator, or $n$-dimensional (for $n > 0$), in which case it has a source
-- | $n-1$ diagram a list of $n-1$ cospans
data Diagram
  = Diagram0 Generator
  | DiagramN { source :: Diagram, cospans :: List Cospan }

-- | A $n$-dimensional diagram (where $n > 0$) can be viewed as a list of
-- | slices of $n-1$ diagrams
type Slices
  = List Diagram

-- | Any diagram can be promoted a dimension, by using it as the source of a
-- | $+1$ dimensional diagram with no cospans
identity :: Diagram -> Diagram
identity d = DiagramN { source: d, cospans: Nil }

-- | Every diagram has a dimension
dimension :: Diagram -> Int
dimension (Diagram0 _) = 0

dimension (DiagramN { source: s }) = 1 + dimension s

-- | Every non-zero-dimensional diagram has a list of cospans
cospans :: Partial => Diagram -> List Cospan
cospans (DiagramN { cospans: cs }) = cs

-- | Every diagram has a size, given by the length of its cospans in the top
-- | dimension
size :: Diagram -> Int
size (Diagram0 _) = 1

size (DiagramN { cospans: cs }) = length cs

-- | Two diagrams are equal when
-- |   * if they are $0$-dimensional, their generators are equal
-- |   * if they are $>0$-dimensional, their source diagrams are equal
instance eqDiagram :: Eq Diagram where
  eq (Diagram0 g) (Diagram0 g') = g == g'
  eq (DiagramN d) (DiagramN d') = d.source == d'.source
  eq _ _ = false

toGenerator :: Partial => Diagram -> Generator
toGenerator (Diagram0 g) = g

toGenerator d = fromJust $ maximum $ map toGenerator $ slices d

type Boundary
  = { source :: Diagram, target :: Diagram }

fromGenerator :: Partial => Maybe Boundary -> Generator -> Diagram
fromGenerator Nothing generator = Diagram0 generator

fromGenerator (Just { source: s, target: t }) generator = DiagramN { source: s, cospans: ({ forwardLimit, backwardLimit } : Nil) }
  where
  forwardLimit = limitCone generator s

  backwardLimit = limitCone generator t

  limitCone :: Generator -> Diagram -> Limit
  limitCone g (Diagram0 base) = Limit0 { sourceGenerator: base, targetGenerator: g }

  limitCone g base@(DiagramN { cospans: cs }) =
    LimitN
      { dimension: dimension base
      , cones:
          ( { index: 0
            , source: cs
            , target: { forwardLimit: limitCone g (source base), backwardLimit: limitCone g (target base) }
            , slices: map (\sl -> limitCone g sl) $ singularSlices base
            }
              : Nil
          )
      }

source :: Partial => Diagram -> Diagram
source (DiagramN { source: s }) = s

target :: Partial => Diagram -> Diagram
target d = fromJust $ last $ slices d

slices :: Partial => Diagram -> Slices
slices d = scanl applyRewrite (source d) $ concatMap genRewrites $ cospans d
  where
  genRewrites :: Partial => Cospan -> List (Diagram -> Diagram)
  genRewrites { forwardLimit: fw, backwardLimit: bw } = rewriteForward fw : rewriteBackward bw : Nil

  applyRewrite :: Diagram -> (Diagram -> Diagram) -> Diagram
  applyRewrite slice rewrite = rewrite slice

  rewriteForward :: Partial => Limit -> Diagram -> Diagram
  rewriteForward (Limit0 { targetGenerator }) (Diagram0 _) = Diagram0 targetGenerator

  rewriteForward LimitI (Diagram0 g) = Diagram0 g

  rewriteForward (LimitN { cones }) (DiagramN d') = DiagramN { source: d'.source, cospans: go d'.cospans 0 cones }
    where
    go :: List Cospan -> Int -> List Cone -> List Cospan
    go cspans _ Nil = cspans

    go cspans i (c : cs) = go (take (c.index + i) cspans <> c.target : drop (c.index + i + coneSize c) cspans) (i - coneSize c + 1) cs

  rewriteBackward :: Limit -> Diagram -> Diagram
  rewriteBackward (Limit0 { sourceGenerator }) (Diagram0 _) = Diagram0 sourceGenerator

  rewriteBackward LimitI (Diagram0 g) = Diagram0 g

  rewriteBackward (LimitN { cones }) (DiagramN d') = DiagramN { source: d'.source, cospans: go d'.cospans 0 cones }
    where
    go :: List Cospan -> Int -> List Cone -> List Cospan
    go cspans _ Nil = cspans

    go cspans i (c : cs) = go (take (c.index + i) cspans <> c.source <> drop (c.index + i + 1) cspans) (i + coneSize c - 1) cs

sliceAt :: Partial => Diagram -> SliceIndex -> Diagram
sliceAt d i = fromJust $ slices d !! heightToIndex (internalHeight)
  where
  internalHeight = internalizeHeight d i

  heightToIndex (Singular h) = h * 2 + 1

  heightToIndex (Regular h) = h * 2

singularSlices :: Partial => Diagram -> Slices
singularSlices d = everyOther (fromJust $ tail (slices d))

regularSlices :: Partial => Diagram -> Slices
regularSlices d = everyOther (slices d)

everyOther :: forall a. List a -> List a
everyOther Nil = Nil

everyOther (x : Nil) = x : Nil

everyOther (x : _ : xs) = x : everyOther xs

internalizeHeight :: Diagram -> SliceIndex -> Height
internalizeHeight _ Source = Regular 0

internalizeHeight d Target = Regular $ size d

internalizeHeight d (Height (Regular h))
  | h < 0 = Regular 0
  | h > size d = Regular $ size d
  | otherwise = Regular h

internalizeHeight d (Height (Singular h))
  | h < 0 = Singular 0
  | h >= size d = Singular $ size d
  | otherwise = Singular h
