module Pred where

import Type.Data.Peano
import Unsafe.Coerce (unsafeCoerce)

foreign import data Pred :: (Nat -> Type) -> Nat -> Type

mkPred :: forall n f. f n -> Pred f (Succ n)
mkPred = unsafeCoerce

getPred :: forall n f. Pred f (Succ n) -> f n
getPred = unsafeCoerce

noPred :: forall a f. Pred f Z -> a
noPred = unsafeCoerce
