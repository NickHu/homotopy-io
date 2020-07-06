{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "homotopy-core"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "enums"
  , "generics-rep"
  , "integers"
  , "lists"
  , "ordered-collections"
  , "psci-support"
  , "spec"
  , "transformers"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
