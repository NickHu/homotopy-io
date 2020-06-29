module Homotopy.Core.Rewrite where

import Data.List (List(..), drop, findMap, length, take, (!!), (:))
import Data.Maybe (fromJust)
import Homotopy.Core.Common (SliceIndex(..), Height(..), Generator)
import Homotopy.Core.Interval (Interval(..))
import Homotopy.Core.Interval as Interval
import Prelude (class Eq, class Semigroup, map, otherwise, ($), (+), (-), (<), (<>), (==), (>=))

-- | An n-dimensional rewrite is a sparsely encoded transformation of
-- | n-dimensional diagrams. Rewrites can contract parts of a diagram and
-- | insert degenerate levels.
data Rewrite
  = Rewrite0 { source :: Generator, target :: Generator }
  | RewriteI
  | RewriteN { dimension :: Int, cones :: List Cone }

derive instance eqRewrite :: Eq Rewrite

-- | A pair of a forward rewrite and a backward rewrite.
-- |
-- | Cospans of rewrites can encode the action of a generator by contracting
-- | the source and target subdiagrams to a point labelled with that generator.
-- | By contracting some levels of the shape of two diagrams such that
-- | non-trivial parts of a diagram are merged with surrounding identity parts,
-- | cospans of rewrites also encode homotopies.
type Cospan
  = { forward :: Rewrite, backward :: Rewrite }

type Cone
  = { index :: Int
    , source :: List Cospan
    , target :: Cospan
    , slices :: List Rewrite
    }

identity :: Int -> Rewrite
identity 0 = RewriteI

identity dim = RewriteN { dimension: dim, cones: Nil }

dimension :: Rewrite -> Int
dimension (Rewrite0 _) = 0

dimension RewriteI = 0

dimension (RewriteN { dimension: dim }) = dim

coneSize :: Cone -> Int
coneSize { source } = length source

slice :: Partial => Rewrite -> Int -> Rewrite
slice (RewriteN { dimension: dim, cones }) height =
  fromJust
    $ findMap (\c -> c.slices !! (height - c.index)) cones

targets :: Partial => Rewrite -> List Int
targets (RewriteN { dimension: dim, cones }) = go cones 0
  where
  go :: List Cone -> Int -> List Int
  go Nil _ = Nil

  go (c : cs) i = (c.index + i) : go cs (i - length c.source + 1)

pad :: List Int -> Rewrite -> Rewrite
pad p (RewriteN { dimension: dim, cones: cs }) =
  RewriteN
    { dimension: dim
    , cones: map (conePad p) cs
    }

pad _ rewrite = rewrite

conePad :: List Int -> Cone -> Cone
conePad Nil cone = cone

conePad (p : ps) cone =
  { index: cone.index + p
  , source: map (cospanPad ps) cone.source
  , target: cospanPad ps cone.target
  , slices: map (pad ps) cone.slices
  }

cospanPad :: List Int -> Cospan -> Cospan
cospanPad p { forward: fw, backward: bw } =
  { forward: pad p fw
  , backward: pad p bw
  }

cospanReverse :: Cospan -> Cospan
cospanReverse cospan = { forward: cospan.backward, backward: cospan.forward }

singularImage :: Partial => Rewrite -> Int -> Int
singularImage (RewriteN { cones }) h = go 0 cones
  where
  go :: Int -> List Cone -> Int
  go i Nil = h + i

  go i (c : cs)
    | h < c.index = h + i
    | h < c.index + coneSize c = c.index + i
    | otherwise = go (i + 1 - coneSize c) cs

singularPreimage :: Partial => Rewrite -> Int -> Interval
singularPreimage (RewriteN { cones }) h = go 0 cones
  where
  go :: Int -> List Cone -> Interval
  go i Nil = Interval { start: h - i, length: 1 }

  go i (c : cs)
    | h < c.index + i = Interval { start: h - i, length: 1 }
    | h == c.index + i = Interval { start: c.index, length: coneSize c }
    | otherwise = go (i + 1 - coneSize c) cs

regularImage :: Partial => Rewrite -> Int -> Int
regularImage (RewriteN { cones }) h = go 0 cones
  where
  go :: Int -> List Cone -> Int
  go i Nil = h - i

  go i (c : cs)
    | h < c.index + i = h - i
    | otherwise = go (i + 1 - coneSize c) cs

regularPreimage :: Partial => Rewrite -> Int -> Interval
regularPreimage lim h =
  let
    left = singularImage lim (h - 1)

    right = singularImage lim h
  in
    Interval { start: left + 1, length: right - left }

transportCoordinates :: Partial => Rewrite -> List SliceIndex -> List (List SliceIndex)
transportCoordinates _ Nil = Nil : Nil

transportCoordinates _ points@(Boundary _ : _) = points : Nil

transportCoordinates lim (Interior (Singular p) : ps) =
  map
    (Interior (Singular (singularImage lim p)) : _)
    $ transportCoordinates (slice lim p) ps

transportCoordinates lim (Interior (Regular p) : ps) =
  map
    (\h -> Interior (Regular h) : ps)
    $ Interval.toUnfoldable
    $ regularPreimage lim p

-- | Composition of rewrites.
-- |
-- | Rewrites of the same dimension and with given source and target diagrams
-- | form a category. In this library, rewrites do not specify their source,
-- | target or dimension in their type or at runtime, so the category of
-- | rewrites is instead presented as a partial semigroup.
instance rewriteSemigroup :: Partial => Semigroup Rewrite where
  append (Rewrite0 { source: fs, target: ft }) (Rewrite0 { source: gs, target: gt })
    | ft == gs = Rewrite0 { source: fs, target: gt }
  append (RewriteN { dimension: fd, cones: fcs }) (RewriteN { dimension: gd, cones: gcs })
    | fd == gd = RewriteN { dimension: fd, cones: composeCones 0 fcs gcs }
      where
      composeCones :: Int -> List Cone -> List Cone -> List Cone
      composeCones _ cs Nil = cs

      composeCones i Nil (c : cs) = c { index = c.index + i } : composeCones i Nil cs

      composeCones i (c : cs) (c' : c's) = case (c.index + i - c'.index) of
        index -- calculate the position of the target of c inside the source range of c'
          | index >= coneSize c' -> c' { index = c'.index + i } : composeCones i (c : cs) c's
          | index < 0 -> c : composeCones (i + 1 - coneSize c) cs (c' : c's)
          | c.target == (fromJust $ c'.source !! index) ->
            composeCones i cs
              ( { index: c'.index + i
                , source: take index c'.source <> c.source <> drop (index + 1) c'.source
                , slices:
                    take index c'.slices
                      <> map (\sl -> sl <> fromJust (c'.slices !! index)) c.slices
                      <> drop (index + 1) c'.slices
                , target: c'.target
                }
                  : c's
              )
