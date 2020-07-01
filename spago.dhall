{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "homotopy-core"
, dependencies =
  [ "console"
  , "effect"
  , "lists"
  , "psci-support"
  , "integers"
  , "transformers"
  , "ordered-collections"
  , "enums"
  , "spec"
  , "aff"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
