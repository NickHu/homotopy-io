-- Compute the diagram with two parallel 2-cell endomorphisms
module Constants.TwoBeads (twoBeads) where

import Prelude
import Data.List
import Homotopy.Core.Common
import Homotopy.Core.Diagram
import Homotopy.Core.Rewrite

gen0 = g 0 0
gen1 = g 1 1
gen2 = g 2 2
objD = d0 gen0
morD = d0 gen1
lim01 = r0 gen0 gen1
lim12 = r0 gen1 gen2
lim02 = r0 gen0 gen2
coneA = c 0 (s lim01 lim01 : Nil) (s lim02 lim02) (r0 gen1 gen2 : Nil)
coneB = c 1 (s lim01 lim01 : Nil) (s lim02 lim02) (r0 gen1 gen2 : Nil)
lim1 = rN 1 (coneA : coneB : Nil)
source = dN (d0 gen0) (s lim01 lim01 : s lim01 lim01 : Nil)

twoBeads :: Diagram
twoBeads = dN source (s lim1 lim1 : Nil)