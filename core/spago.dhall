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
  , "graphs"
  , "integers"
  , "lazy"
  , "lists"
  , "ordered-collections"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "spec"
  , "transformers"
  , "tree"
  , "unordered-collections"
  , "unsafe-reference"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
