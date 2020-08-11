module Homotopy.Core.Normalization.Degeneracy where

import Prelude

import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Homotopy.Core.Interval (Interval(..))

data Degeneracy = Degeneracy (List Int) (Map Int Degeneracy)

slice :: Partial => Degeneracy -> Int -> Degeneracy
slice degeneracy@(Degeneracy _ slices) height =
  case Map.lookup (singularImage degeneracy height) slices of
    Just s -> s
    Nothing -> identity

singularImage :: Degeneracy -> Int -> Int
singularImage (Degeneracy heights _) height = go 0 heights
  where
  go i = case _ of
    Nil -> height + i
    h : hs
      | height + i < h -> height + i
      | otherwise -> go (i + 1) hs

singularPreimage :: Degeneracy -> Int -> Interval
singularPreimage (Degeneracy heights _) height = go 0 heights
  where
  go i = case _ of
    Nil -> Interval { start: height - i, length: 1 }
    h : hs
      | height < h -> Interval { start: height - i, length: 1 }
      | height == h -> Interval { start: height - i, length: 0 }
      | otherwise -> go (i + 1) hs

identity :: Degeneracy
identity = Degeneracy Nil mempty
