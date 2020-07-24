{ name = "homotopy-webclient"
, dependencies =
  [ "console"
  , "effect"
  , "numbers"
  , "record"
  , "debug"
  , "refs"
  , "homotopy-core"
  , "concur-react"
  , "concur-core"
  , "unsafe-reference"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
