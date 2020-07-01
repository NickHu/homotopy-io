module Homotopy.Core.Interval (Interval(..), toUnfoldable) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Prelude (class Eq, class Show, otherwise, (+), (-), (<<<), (==))

newtype Interval
  = Interval { start :: Int, length :: Int }

derive newtype instance eqInterval :: Eq Interval

derive instance newtypeInterval :: Newtype Interval _

derive instance genericInterval :: Generic Interval _

instance showInterval :: Show Interval where
  show x = genericShow x

toUnfoldable :: forall f. Unfoldable f => Interval -> f Int
toUnfoldable = unfoldr step <<< unwrap
  where
  step interval
    | interval.length == 0 = Nothing
    | otherwise = Just (Tuple interval.start { start: interval.start + 1, length: interval.length - 1 })
