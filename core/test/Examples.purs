module Test.Examples where

import Data.List (List(..), (:))
import Data.Maybe (fromJust)
import Homotopy.Core.Common (Boundary(..), Generator(..))
import Homotopy.Core.Diagram (Diagram(..), DiagramN, make)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Rewrite (Rewrite(..), Cospan, makeRewriteN)
import Partial.Unsafe (unsafePartial)
import Prelude (($))

--     |          |
--     m          m
--    / \        / \
--   |   |  ⤳   |   |
--   m   |      |   m
--  / \  |      |  / \
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

-- | |
-- | f
-- | |
-- f |
-- | |
twoBeadsTwoLevels :: DiagramN
twoBeadsTwoLevels =
  let
    attach b e s l = unsafePartial $ fromJust $ Diagram.attach b e s l

    spaceG :: Generator
    spaceG = Generator { id: 0, dimension: 0 }

    wireG :: Generator
    wireG = Generator { id: 1, dimension: 1 }

    fG :: Generator
    fG = Generator { id: 2, dimension: 2 }

    space :: Diagram
    space = Diagram0 spaceG

    wire :: DiagramN
    wire = Diagram.fromGenerator space space wireG

    f :: DiagramN
    f = Diagram.fromGenerator (DiagramN wire) (DiagramN wire) fG

    bottom :: DiagramN
    bottom = attach Target Nil wire f

    top :: DiagramN
    top = attach Source Nil wire f

    beads :: DiagramN
    beads = attach Target Nil top bottom
  in
    beads

-- | |
-- f f
-- | |
twoBeadsOneLevel :: DiagramN
twoBeadsOneLevel =
  let
    attach b e s l = unsafePartial $ fromJust $ Diagram.attach b e s l

    spaceG :: Generator
    spaceG = Generator { id: 0, dimension: 0 }

    wireG :: Generator
    wireG = Generator { id: 1, dimension: 1 }

    fG :: Generator
    fG = Generator { id: 2, dimension: 2 }

    space :: Diagram
    space = Diagram0 spaceG

    wire :: DiagramN
    wire = Diagram.fromGenerator space space wireG

    f :: DiagramN
    f = Diagram.fromGenerator (DiagramN wire) (DiagramN wire) fG

    wiretimeswire :: DiagramN
    wiretimeswire = attach Target Nil wire wire

    -- s -> w
    stow :: Rewrite
    stow = Rewrite0 { source: spaceG, target: wireG }

    -- s -> w <- s
    stowCospan :: Cospan
    stowCospan = { forward: stow, backward: stow }

    -- s -> f
    stof :: Rewrite
    stof = Rewrite0 { source: spaceG, target: fG }

    -- s -> f <- s
    stofCospan :: Cospan
    stofCospan = { forward: stof, backward: stof }

    -- f
    -- ^
    -- |
    -- w
    wtof :: Rewrite
    wtof = Rewrite0 { source: wireG, target: fG }

    -- s -> f <- s -> f <- s
    --      ^         ^
    --      |         |
    -- s -> w <- s -> w <- s
    both :: Rewrite
    both = makeRewriteN 1 $ { index: 0, source: stowCospan : Nil, target: stofCospan, slices: wtof : Nil } : { index: 1, source: stowCospan : Nil, target: stofCospan, slices: wtof : Nil } : Nil
  in
    unsafePartial $ fromJust $ make (DiagramN wiretimeswire) ({ forward: both, backward: both } : Nil)

-- | |     | |
-- | f     | |
-- | |  ⤳  f f
-- f |     | |
-- | |     | |
twoBeadsTwoLevelsToOne :: Rewrite
twoBeadsTwoLevelsToOne =
  let
    attach b e s l = unsafePartial $ fromJust $ Diagram.attach b e s l

    spaceG :: Generator
    spaceG = Generator { id: 0, dimension: 0 }

    wireG :: Generator
    wireG = Generator { id: 1, dimension: 1 }

    fG :: Generator
    fG = Generator { id: 2, dimension: 2 }

    space :: Diagram
    space = Diagram0 spaceG

    wire :: DiagramN
    wire = Diagram.fromGenerator space space wireG

    f :: DiagramN
    f = Diagram.fromGenerator (DiagramN wire) (DiagramN wire) fG

    wiretimeswire :: DiagramN
    wiretimeswire = attach Target Nil wire wire

    -- s -> w
    stow :: Rewrite
    stow = Rewrite0 { source: spaceG, target: wireG }

    -- s -> w <- s
    stowCospan :: Cospan
    stowCospan = { forward: stow, backward: stow }

    -- s -> f
    stof :: Rewrite
    stof = Rewrite0 { source: spaceG, target: fG }

    -- s -> f <- s
    stofCospan :: Cospan
    stofCospan = { forward: stof, backward: stof }

    -- f
    -- ^
    -- |
    -- w
    wtof :: Rewrite
    wtof = Rewrite0 { source: wireG, target: fG }

    -- s -> f <- s -> f <- s
    --      ^         ^
    --      |         |
    -- s -> w <- s -> w <- s
    both :: Rewrite
    both = makeRewriteN 1 $ { index: 0, source: stowCospan : Nil, target: stofCospan, slices: wtof : Nil } : { index: 1, source: stowCospan : Nil, target: stofCospan, slices: wtof : Nil } : Nil

    -- s -> f <- s -> w <- s
    --      ^         |
    --      |         |
    -- s -> w <- s -> w <- s
    left :: Rewrite
    left = makeRewriteN 1 $ { index: 0, source: stowCospan : Nil, target: stofCospan, slices: wtof : Nil } : Nil

    -- s -> w <- s -> f <- s
    --      |         ^
    --      |         |
    -- s -> w <- s -> w <- s
    right :: Rewrite
    right = makeRewriteN 1 $ { index: 1, source: stowCospan : Nil, target: stofCospan, slices: wtof : Nil } : Nil
  in
    makeRewriteN 2
      $ { index: 0
        , source: { forward: left, backward: left } : { forward: right, backward: right } : Nil
        , target: { forward: both, backward: both }
        , slices:
            ( makeRewriteN 1
                $ { index: 1
                  , source: stowCospan : Nil
                  , target: stofCospan
                  , slices: wtof : Nil
                  }
                : Nil
            )
              : ( makeRewriteN 1
                    $ { index: 0
                      , source: stowCospan : Nil
                      , target: stofCospan
                      , slices: wtof : Nil
                      }
                    : Nil
                )
              : Nil
        }
      : Nil
