module Diagram where
-- module Diagram (
--   identity,
--   dimension,
--   size,
--   cospans,
--   toGenerator,
--   fromGenerator,
--   source,
--   target,
--   slices,
--   sliceAt,
--   singularSlices,
--   regularSlices,
--   internalizeHeight
--   ) where

import Data.List
import Data.Maybe
import Data.Newtype
import ExistsNat
import Prelude
import Type.Data.Peano

import Generator (Generator)
import Generator as G
import Limit (Limit(..), Cospan(..), Cospans(..), SliceIndex(..), Height(..))
import Limit as L
import Type.Prelude (EQ, True)
import Unsafe.Coerce (unsafeCoerce)

-- data Dgram = Dgram0 Generator | DgramN { source :: Dgram, cospans :: Cospans }
--
-- newtype Diagram (n :: Nat) = Diagram Dgram
--
data Diagram (n :: Nat) =
    Diagram0 (IsZeroNat n True => Generator)
  | DiagramN { source :: (ExistsNat (Lower Diagram n)), cospans :: (ExistsNat (Lower Cospans n)) }

-- class IsDiagram rep where
--   diagram0 :: Generator -> rep Z
--   diagramN :: forall n. rep n -> Cospans n -> rep (Succ n)
--
-- type Diagram n = forall rep. IsDiagram rep => rep n
--
-- data D (n :: Nat) = D0 Generator | DN { source :: Diagram n, cospans :: Cospans n }
--
-- instance dDiagram :: IsDiagram D where
--   diagram0 generator = D0 generator
--   diagramN source cospans = DN { source, cospans }

diagram0 :: Generator -> Diagram Z
diagram0 g = Diagram0 g

diagramN :: forall n. Diagram n -> Cospans n -> Diagram (Succ n)
diagramN d cs = DiagramN { source: mkExistsNat (Lower d), cospans: mkExistsNat (Lower cs) }

type Slices n = List (Diagram n)

identity :: forall n. Diagram n -> Diagram (Succ n)
identity d = diagramN d (Cospans Nil)

dimension :: forall n. IsNat n => Diagram n -> Int
dimension d = reflectNat (toProxy d)
  where toProxy :: Diagram n -> NProxy n
        toProxy = unsafeCoerce unit

-- It's not really partial, as it shouldn't be possible to construct
-- `Diagram0 ... :: Diagram (Succ n)` for any n
cospans :: Partial => forall n. Diagram (Succ n) -> Cospans n
cospans (DiagramN { cospans: cs }) = unLower $ getExistsNat cs
-- why doesn't this work?
-- cospans (DiagramN { cospans: cs }) = runExistsNat (unLower :: forall n m. Lower Cospans (Succ n) m -> Cospans n) cs

-- unLower :: Lower Cospans (Succ n) m -> (Pred (Succ n) m => Cospans m)

-- Have cs :: ExistsNat (Lower f (Succ n))
-- getExistsNat cs :: Lower f (Succ n) m
-- unLower $ getExistsNat cs :: SumNat (Succ Z) (Succ n) m => f m
-- runExistsNat (unLower :: forall m. Lower f (Succ n) m -> f m) (cs :: ExistsNat (Lower f (Succ n))) :: f m

-- class ElimNat (k :: Nat) where
--   elimNat :: forall f. f Z -> (forall n. f (Succ n)) -> f k
--
-- instance ez :: ElimNat Z where
--   elimNat f _ = f
--
-- instance es :: ElimNat n => ElimNat (Succ n) where
--   elimNat _ f = f
--
-- size :: forall n. ElimNat n => Diagram n -> Int
-- size d = elimNat (const 1 :: Diagram Z -> Int) (\diagram -> length $ unwrap $ cospans diagram) d

-- class Size x where
--   size :: x -> Int
--
-- instance dzSize :: Size (Diagram Z) where
--   size _ = 1
--
-- instance dsSize :: Partial => Size (Diagram (Succ n)) where
--   size d = length $ unwrap $ cospans d

size :: forall n. Diagram n -> Int
size (Diagram0 _) = 1
size (DiagramN { source, cospans }) = ?hole $ unLower $ getExistsNat cospans

-- size d = case dimension d of
--   0 -> 1
--   _ -> length $ cospans (d :: forall m. Diagram (Succ m))
--
-- -- equal :: Diagram -> Diagram -> Boolean
-- -- equal (Diagram0 g) (Diagram0 g') = Generator.equal g g'
-- -- equal (DiagramN d) (DiagramN d') = hash d == hash d' && equal d.source d'.source -- TODO
-- -- equal _ _ = false
--
--
class ToGenerator d where
  toGenerator :: d -> Generator

instance dzToGenerator :: Partial => ToGenerator (Diagram Z) where
  toGenerator (Diagram0 g) = g

-- instance dsToGenerator :: ToGenerator (Diagram (Succ n)) where
--   toGenerator d = max $ map toGenerator $ slices d

-- toGenerator :: Diagram n -> Generator
-- toGenerator (Diagram (Dgram0 g)) = g
-- toGenerator (Diagram (DgramN d)) = max (map toGenerator (slices d))

-- fromGenerator :: Generator -> Diagram n
-- fromGenerator = unsafeCoerce unit -- TODO
--

source :: Partial => forall n. Diagram (Succ n) -> Diagram n
source (DiagramN { source }) = unLower $ getExistsNat source

-- target :: Diagram (Succ n) -> Diagram n
-- target d = fromJust $ last $ slices d

-- slices :: Diagram (Succ n) -> Slices
-- slices d = scanl applyRewrite (source d) $ concat $ map genRewrites $ cospans d
--   where
--   genRewrites :: Cospan n -> List (Diagram n -> Diagram n)
--   genRewrites { forwardLimit: fw, backwardLimit: bw } = [rewriteForward fw, rewriteBackward bw]
--   applyRewrite :: Diagram n -> (Diagram n -> Diagram n) -> Diagram n
--   applyRewrite slice rewrite = rewrite slice
--   rewriteForward :: Limit n -> Diagram n -> Diagram n
--   rewriteForward limit diagram = case dimension diagram of
--                                       0 -> L.targetGenerator limit
--                                       _ -> diagramN $ unsafeCoerce unit -- TODO
--   rewriteBackward :: Limit n -> Diagram n -> Diagram n
--   rewriteBackward limit diagram = case dimension diagram of
--                                       0 -> L.targetGenerator limit
--                                       _ -> diagramN $ unsafeCoerce unit -- TODO
  -- rewriteForward (Limit0 { targetGenerator }) (Diagram0 _) = Diagram0 targetGenerator
  -- rewriteForward LimitI (Diagram0 g) = Diagram0 g
  -- rewriteForward (LimitN { cones }) (DiagramN d) = DiagramN { source: d.source, cospans: cospans, hash: 0 }
  --                                                    where cospans = go d.cospans 0 cones
  --                                                          go :: Cospans -> Index -> Cones -> Cospans
  --                                                          go cospans _ [] = cospans
  --                                                          go cospans i (c:cs) = go (fromJust $ insertAt (c.index + i) c.target cospans) (i - Limit.coneSize c + 1) cs
  -- rewriteBackward :: Limit -> Diagram -> Diagram
  -- rewriteBackward (Limit0 { sourceGenerator}) (Diagram0 _) = Diagram0 sourceGenerator
  -- rewriteBackward LimitI (Diagram0 g) = Diagram0 g
  -- rewriteBackward (LimitN { cones }) (DiagramN d) = DiagramN { source: d.source, cospans: cospans, hash: 0 }
  --                                                     where cospans = go d.cospans 0 cones
  --                                                           go :: Cospans -> Index -> Cones -> Cospans
  --                                                           go cospans _ [] = cospans
  --                                                           go cospans i (c:cs) = go (fromJust $ insertAt (c.index + i) c.source cospans) (i + Limit.coneSize c - 1) cs
--
-- sliceAt :: Diagram (Succ n) -> SliceIndex -> Diagram n
-- sliceAt d i = ?sliceAthole
--
-- singularSlices :: Diagram (Succ n) -> Slices
-- singularSlices d = everyOther (tail (slices d))

-- regularSlices :: Diagram (Succ n) -> Slices
-- regularSlices d = everyOther (slices d)

everyOther :: forall a. List a -> List a
everyOther Nil = Nil
everyOther (x:Nil) = x : Nil
everyOther (x:_:xs) = x : everyOther xs

internalizeHeight :: Partial => forall n. Diagram (Succ n) -> SliceIndex -> Height
internalizeHeight _ Source = Regular 0
internalizeHeight d Target = Regular $ size d
internalizeHeight d (H (Regular h))
  | h < 0 = Regular 0
  | h > size d = Regular $ size d
  | otherwise = Regular h
internalizeHeight d (H (Singular h))
  | h < 0 = Singular 0
  | h >= size d = Singular $ size d
  | otherwise = Singular h
--
