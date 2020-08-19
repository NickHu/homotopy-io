module Test.Diagram where

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.MonadZero (empty)
import Data.Array as Array
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error, error)
import Homotopy.Core.Common (Boundary(..), Generator(..), Height(..), SliceIndex(..))
import Homotopy.Core.Diagram (Diagram(..), attach, fromGenerator, make)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Rewrite (Rewrite(..), makeRewriteN)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, map, mod, pure, ($), (-), (/), (==))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

generatorAt :: Diagram -> List SliceIndex -> Maybe Generator
generatorAt (Diagram0 generator) Nil = Just generator

generatorAt (DiagramN diagram) (h : hs) = do
  slice <- Diagram.sliceAt diagram h
  generatorAt slice hs

generatorAt _ _ = Nothing

shouldHavePoints :: forall m. MonadThrow Error m => Diagram -> Array (Tuple (Array Int) Generator) -> m Unit
shouldHavePoints diagram points =
  for_ points \(Tuple point generator) -> do
    generatorAt diagram (Array.toUnfoldable $ map toHeight point) `shouldEqual` Just generator
  where
  toHeight i = Interior $ if mod i 2 == 0 then Regular (i / 2) else Singular ((i - 1) / 2)

tryMaybe :: forall m a. MonadThrow Error m => String -> Maybe a -> m a
tryMaybe _ (Just x) = pure x

tryMaybe message Nothing = throwError (error message)

main :: Spec Unit
main = do
  describe "smart constructor" do
    let
      s = Generator { id: 0, dimension: 0 }

      -- -> x <-
      x = Generator { id: 1, dimension: 1 }

      -- -> y <-
      y = Generator { id: 2, dimension: 1 }

      -- x
      -- |
      -- f
      -- |
      -- x
      f = Generator { id: 3, dimension: 2 }

      -- y
      -- |
      -- g
      -- |
      -- y
      g = Generator { id: 4, dimension: 2 }

      space = Diagram0 s

      xwire = fromGenerator space space x

      ywire = fromGenerator space space y

      sxs = { forward: Rewrite0 { source: s, target: x }, backward: Rewrite0 { source: s, target: x } }

      sys = { forward: Rewrite0 { source: s, target: y }, backward: Rewrite0 { source: s, target: y } }

      sfs = { forward: Rewrite0 { source: s, target: f }, backward: Rewrite0 { source: s, target: f } }

      sgs = { forward: Rewrite0 { source: s, target: g }, backward: Rewrite0 { source: s, target: g } }

      xtof = Rewrite0 { source: x, target: f }

      ytog = Rewrite0 { source: x, target: g }

      xy = unsafePartial $ fromJust $ attach Target Nil xwire ywire
    it "accepts compatible cospans" do
      -- x
      -- |
      -- f
      -- |
      -- f
      -- |
      -- x
      unsafePartial
        $ make (DiagramN xwire)
            ( { forward:
                  makeRewriteN 1
                    ( { index: 0
                      , source: sxs : Nil
                      , target: sfs
                      , slices: xtof : Nil
                      }
                        : Nil
                    )
              , backward:
                  makeRewriteN 1
                    ( { index: 0
                      , source: sxs : Nil
                      , target: sfs
                      , slices: xtof : Nil
                      }
                        : Nil
                    )
              }
                : { forward:
                      makeRewriteN 1
                        ( { index: 0
                          , source: sxs : Nil
                          , target: sfs
                          , slices: xtof : Nil
                          }
                            : Nil
                        )
                  , backward:
                      makeRewriteN 1
                        ( { index: 0
                          , source: sxs : Nil
                          , target: sfs
                          , slices: xtof : Nil
                          }
                            : Nil
                        )
                  }
                : Nil
            )
            `shouldEqual`
              (let d = fromGenerator (DiagramN xwire) (DiagramN xwire) f in attach Target Nil d d)
    it "fails when forward and backward rewrites do not agree (singular heights are not well-defined)" do
      -- y x
      -- | |
      -- ? ?
      -- | |
      -- x y
      unsafePartial
        $ make (DiagramN xy)
            ( { forward:
                  makeRewriteN 1
                    ( ( { index: 0
                        , source: sxs : Nil
                        , target: sfs
                        , slices: xtof : Nil
                        }
                          : { index: 1
                            , source: sys : Nil
                            , target: sgs
                            , slices: ytog : Nil
                            }
                          : Nil
                      )
                    )
              , backward:
                  makeRewriteN 1
                    ( ( { index: 0
                        , source: sys : Nil
                        , target: sgs
                        , slices: ytog : Nil
                        }
                          : { index: 1
                            , source: sxs : Nil
                            , target: sfs
                            , slices: xtof : Nil
                            }
                          : Nil
                      )
                    )
              }
                : Nil
            )
            `shouldEqual`
              empty
    it "fails when cospans do not compose (regular heights are not well-defined)" do
      -- y
      -- |
      -- g
      -- |
      -- ?
      -- |
      -- f
      -- |
      -- x
      unsafePartial
        $ make (DiagramN xwire)
            ( { forward:
                  makeRewriteN 1
                    ( ( { index: 0
                        , source: sxs : Nil
                        , target: sfs
                        , slices: xtof : Nil
                        }
                          : Nil
                      )
                    )
              , backward:
                  makeRewriteN 1
                    ( ( { index: 0
                        , source: sxs : Nil
                        , target: sfs
                        , slices: xtof : Nil
                        }
                          : Nil
                      )
                    )
              }
                : { forward:
                      makeRewriteN 1
                        ( ( { index: 0
                            , source: sys : Nil
                            , target: sgs
                            , slices: ytog : Nil
                            }
                              : Nil
                          )
                        )
                  , backward:
                      makeRewriteN 1
                        ( ( { index: 0
                            , source: sys : Nil
                            , target: sgs
                            , slices: ytog : Nil
                            }
                              : Nil
                          )
                        )
                  }
                : Nil
            )
            `shouldEqual`
              empty
  describe "1-d composition" do
    let
      x = Generator { id: 0, dimension: 0 }

      y = Generator { id: 1, dimension: 0 }

      z = Generator { id: 2, dimension: 0 }

      f = Generator { id: 3, dimension: 1 }

      g = Generator { id: 4, dimension: 1 }

      fd = Diagram.fromGenerator (Diagram0 x) (Diagram0 y) f

      gd = Diagram.fromGenerator (Diagram0 y) (Diagram0 z) g
    it "compatible composition at target succeeds" do
      result <- tryMaybe "attachment failed" $ attach Target Nil gd fd
      DiagramN result `shouldHavePoints` [ [ 0 ] /\ x, [ 1 ] /\ f, [ 2 ] /\ y, [ 3 ] /\ g, [ 4 ] /\ z ]
    it "compatible composition at source succeeds" do
      result <- tryMaybe "attachment failed" $ attach Source Nil fd gd
      DiagramN result `shouldHavePoints` [ [ 0 ] /\ x, [ 1 ] /\ f, [ 2 ] /\ y, [ 3 ] /\ g, [ 4 ] /\ z ]
    it "non-compatible composition at target fails" do
      (isNothing $ attach Target Nil fd gd) `shouldEqual` true
    it "non-compatible composition at source fails" do
      (isNothing $ attach Source Nil gd fd) `shouldEqual` true
