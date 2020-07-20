{ name = "homotopy-webclient"
, dependencies =
  [ "console"
  , "effect"
  , "homotopy-core"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
