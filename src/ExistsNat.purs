module ExistsNat where

-- same as
-- https://github.com/purescript/purescript-exists/blob/master/src/Data/Exists.purs,
-- but for the `Nat` kind instead of `Type`

import Prim.Boolean
import Type.Data.Peano
import Type.Data.Peano.Nat.Definition (class Pred)

import Data.Newtype (class Newtype)
import Unsafe.Coerce (unsafeCoerce)

foreign import data ExistsNat :: (Nat -> Type) -> Type

mkExistsNat :: forall f a. f a -> ExistsNat f
mkExistsNat = unsafeCoerce

runExistsNat :: forall f r. (forall a. f a -> r) -> ExistsNat f -> r
runExistsNat = unsafeCoerce

getExistsNat :: forall f a. ExistsNat f -> f a
getExistsNat = unsafeCoerce

-- `Pred n m` means "the predecessor of n is m", i.e. n = m + 1
newtype Lower f n m = Lower (Pred n m => f m)
-- derive instance newtypeLower :: Newtype (Lower f n m) _
-- instance newtypeLower :: Newtype (Lower f n m) (f m) where
--   wrap = Lower
--   unwrap (Lower x) = x

unLower :: forall f n m. Lower f n m -> (Pred n m => f m)
unLower (Lower x) = x

-- to express something like `MyType (Pred n)` as an existential type,
-- use `ExistsNat (Lower MyType n)`

-- class NatEquals (a :: Nat) (b :: Nat) | a -> b, b -> a
--
-- instance reflNat :: NatEquals a a
