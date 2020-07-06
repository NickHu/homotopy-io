module Test.Diagram where

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array as Array
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error, error)
import Homotopy.Core.Common (Boundary(..), Generator(..), Height(..), SliceIndex(..))
import Homotopy.Core.Diagram (Diagram(..), attach)
import Homotopy.Core.Diagram as Diagram
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
    it "non-comaptible composition at source fails" do
      (isNothing $ attach Source Nil gd fd) `shouldEqual` true
