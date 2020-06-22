module Limit where

import Data.List (List(..), drop, findMap, length, take, (!!), (..), (:))
import Data.Maybe (fromJust)
import Prelude (class Eq, class Semigroup, map, otherwise, ($), (+), (-), (<), (<>), (==), (>=))
import Generator (Generator)

data Limit
  = Limit0 { sourceGenerator :: Generator, targetGenerator :: Generator }
  | LimitI
  | LimitN { dimension :: Int, cones :: List Cone }

derive instance eqLimit :: Eq Limit

type Cospan
  = { forwardLimit :: Limit, backwardLimit :: Limit }

type Cone
  = { index :: Int
    , source :: List Cospan
    , target :: Cospan
    , slices :: List Limit
    }

data Height
  = Regular Int
  | Singular Int

data SliceIndex
  = Height Height
  | Source
  | Target

identity :: Int -> Limit
identity 0 = LimitI

identity dim = LimitN { dimension: dim, cones: Nil }

dimension :: Limit -> Int
dimension (Limit0 _) = 0

dimension LimitI = 0

dimension (LimitN { dimension: dim }) = dim

coneSize :: Cone -> Int
coneSize { source } = length source

slice :: Partial => Limit -> Int -> Limit
slice (LimitN { dimension: dim, cones }) height =
  fromJust
    $ findMap (\c -> c.slices !! (height - c.index)) cones

targets :: Partial => Limit -> List Int
targets (LimitN { dimension: dim, cones }) = go cones 0
  where
  go :: List Cone -> Int -> List Int
  go Nil _ = Nil

  go (c : cs) i = (c.index + i) : go cs (i - length c.source + 1)

pad :: List Int -> Limit -> Limit
pad p (LimitN { dimension: dim, cones: cs }) =
  LimitN
    { dimension: dim
    , cones: map (conePad p) cs
    }

pad _ limit = limit

conePad :: List Int -> Cone -> Cone
conePad Nil cone = cone

conePad (p : ps) cone =
  { index: cone.index + p
  , source: map (cospanPad ps) cone.source
  , target: cospanPad ps cone.target
  , slices: map (pad ps) cone.slices
  }

cospanPad :: List Int -> Cospan -> Cospan
cospanPad p { forwardLimit: fw, backwardLimit: bw } =
  { forwardLimit: pad p fw
  , backwardLimit: pad p bw
  }

singularImage :: Partial => Limit -> Int -> Int
singularImage (LimitN { cones }) h = go 0 cones
  where
  go :: Int -> List Cone -> Int
  go i Nil = h + i

  go i (c : cs)
    | h < c.index = h + i
    | h < c.index + coneSize c = c.index + i
    | otherwise = go (i + 1 - coneSize c) cs

singularPreimage :: Partial => Limit -> Int -> List Int
singularPreimage (LimitN { cones }) h = go 0 cones
  where
  go :: Int -> List Cone -> List Int
  go i Nil = (h - i) : Nil

  go i (c : cs)
    | h < c.index + i = (h - i) : Nil
    | h == c.index + i = c.index .. (c.index + coneSize c)
    | otherwise = go (i + 1 - coneSize c) cs

regularImage :: Partial => Limit -> Int -> Int
regularImage (LimitN { cones }) h = go 0 cones
  where
  go :: Int -> List Cone -> Int
  go i Nil = h - i

  go i (c : cs)
    | h < c.index + i = h - i
    | otherwise = go (i + 1 - coneSize c) cs

regularPreimage :: Partial => Limit -> Int -> List Int
regularPreimage lim h =
  let
    left = singularImage lim (h - 1)

    right = singularImage lim h
  in
    (left + 1) .. (right + 1)

transportCoordinates :: Partial => Limit -> List SliceIndex -> List (List SliceIndex)
transportCoordinates _ Nil = Nil : Nil

transportCoordinates _ points@(Source : _) = points : Nil

transportCoordinates _ points@(Target : _) = points : Nil

transportCoordinates lim (Height (Singular p) : ps) =
  map
    (Height (Singular (singularImage lim p)) : _)
    $ transportCoordinates (slice lim p) ps

transportCoordinates lim (Height (Regular p) : ps) =
  map
    (\h -> Height (Regular h) : ps)
    $ regularPreimage lim p

instance limitSemigroup :: Partial => Semigroup Limit where
  append (Limit0 { sourceGenerator: fs, targetGenerator: ft }) (Limit0 { sourceGenerator: gs, targetGenerator: gt })
    | ft == gs = Limit0 { sourceGenerator: fs, targetGenerator: gt }
  append (LimitN { dimension: fd, cones: fcs }) (LimitN { dimension: gd, cones: gcs })
    | fd == gd = LimitN { dimension: fd, cones: composeCones 0 fcs gcs }
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
