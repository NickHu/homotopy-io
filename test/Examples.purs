module Test.Examples where

import Homotopy.Core.Diagram (DiagramN, Diagram(..))
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Common (Generator(..), Boundary(..))
import Data.List (List(..))
import Prelude (($))
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust)

associativity :: DiagramN
associativity =
  let
    attach b e s l = unsafePartial $ fromJust $ Diagram.attach b e s l

    x = Generator { id: 0, dimension: 0 }

    f = Generator { id: 1, dimension: 1 }

    m = Generator { id: 2, dimension: 2 }

    a = Generator { id: 3, dimension: 3 }

    fd = Diagram.fromGenerator (Diagram0 x) (Diagram0 x) f

    ffd = attach Target Nil fd fd

    md = Diagram.fromGenerator (DiagramN ffd) (DiagramN fd) m

    mfd = attach Target Nil fd md

    ld = attach Target Nil md mfd

    fmd = attach Source Nil fd md

    rd = attach Target Nil md fmd

    ad = Diagram.fromGenerator (DiagramN ld) (DiagramN rd) a
  in
    ad
