module Limit where

import Prelude

import Data.List
import Data.Newtype
import Type.Data.Peano

import Generator (Generator)

-- data Lim =
--     Lim0 { sourceGenerator :: Generator, targetGenerator :: Generator }
--   | LimI
--   | LimN { cones :: Cones n }
--
-- newtype Limit (n :: Nat) = Limit Lim

data Limit (n :: Nat) =
    Limit0 { sourceGenerator :: Generator, targetGenerator :: Generator }
  | LimitI
  | LimitN { cones :: Cones n }

limit0 :: Generator -> Generator -> Limit Z
limit0 s t = Limit0 { sourceGenerator: s, targetGenerator: t }

limitI :: forall n. Limit n
limitI = LimitI

limitN :: forall n. Cones n -> Limit n
limitN cs = LimitN { cones: cs }

type Limits n = List (Limit n)

type Cospan n = { forwardLimit :: Limit n, backwardLimit :: Limit n }

newtype Cospans n = Cospans (List (Cospan n))
derive instance newtypeCospans :: Newtype (Cospans n) _

type Index = Int

type Cone n = {
  index :: Index,
  source :: Cospans n,
  target :: Cospan n,
  slices :: Limits n
}

type Cones n = List (Cone n)

data Height = Regular Int | Singular Int

data SliceIndex = H Height | Source | Target

-- coneSize :: forall n. Cone (Succ n) -> Int
-- coneSize (LimitN { source }) = length source
