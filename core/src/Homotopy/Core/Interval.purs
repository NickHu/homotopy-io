module Homotopy.Core.Interval (Interval(..), toUnfoldable, isEmpty, image) where

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

isEmpty :: Interval -> Boolean
isEmpty (Interval interval) = interval.length == 0

image :: (Int -> Interval) -> Interval -> Interval
image f (Interval interval) =
  case interval.length of
    0 -> Interval { start: (unwrap (f interval.start)).start, length: 0 }
    1 -> f interval.start
    _ -> Interval {
      start: (unwrap (f interval.start)).start,
      length: (unwrap (f (interval.start + interval.length - 1))).length
    }
