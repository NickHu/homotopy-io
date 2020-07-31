{ name = "homotopy-webclient"
, dependencies =
  [ "console"
  , "effect"
  , "numbers"
  , "record"
  , "debug"
  , "refs"
  , "homotopy-core"
  , "react-basic-hooks"
  , "react-basic"
  , "react-basic-dom"
  , "profunctor-lenses"
  , "unsafe-reference"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
