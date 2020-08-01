module Homotopy.Webclient.Lenses where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

signature :: forall r a. Lens' { signature :: a | r } a
signature = prop (SProxy :: SProxy "signature")

workspace :: forall r a. Lens' { workspace :: a | r } a
workspace = prop (SProxy :: SProxy "workspace")

path :: forall r a. Lens' { path :: a | r } a
path = prop (SProxy :: SProxy "path")

generators :: forall r a. Lens' { generators :: a | r } a
generators = prop (SProxy :: SProxy "generators")

name :: forall r a. Lens' { name :: a | r } a
name = prop (SProxy :: SProxy "name")

color :: forall r a. Lens' { color :: a | r } a
color = prop (SProxy :: SProxy "color")
