module Homotopy.Core.MiniMake (g, r0, rI, rN, d0, dN, s, c) where

import Homotopy.Core.Common
import Homotopy.Core.Rewrite
import Homotopy.Core.Diagram
import Data.List

-- Mini unsafe constructors

g :: Int -> Int -> Generator
g a b = Generator { id:a, dimension:b }

r0 :: Generator -> Generator -> Rewrite
r0 a b = Rewrite0 { source: a, target: b }

rI :: Rewrite
rI = RewriteI

rN :: Int -> List Cone -> Rewrite
rN a b = RewriteN { dimension: a, cones: b }

s :: Rewrite -> Rewrite -> Cospan
s x y = { forward:x, backward:x }

c :: Int -> List Cospan -> Cospan -> List Rewrite -> Cone
c w x y z = { index:w, source:x, target:y, slices:z }

d0 :: Generator -> Diagram
d0 a = Diagram0 a

dN :: Diagram -> List Cospan -> Diagram
dN a b = DiagramN (unsafeMake a b)

-- Mini display functions
{-
miniShow :: Generator -> String
  miniShow (Generator g) = "g " <> show g.id <> " " <> show g.dimension

miniShow :: Rewrite -> String
  miniShow (Rewrite0 { source: s, target: t }) = "r0 (" <> miniShow s <> ") (" <> miniShow t <> ")"
  miniShow (RewriteN { dimension: d, cones: c }) = "rN (" <> miniShow d <> ") (" <> miniShow c <> ")"
  miniShow RewriteI = "rI"

    instance showDiagramN :: Show DiagramN where
  show x = genericShow x
  show (InternalDiagram { source: s, cospans: c }) = "dN (" <> (show s) <> ") (" <> (show c) <> ")"

instance showDiagram :: Show Diagram where
  --show x = genericShow x
  show (Diagram0 a) = "d0 (" <> (show a) <> ")"
  show (DiagramN a) = show a

-}