{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "homotopy-webclient"
, dependencies = 
  [ "console"
  , "effect"
  , "psci-support"
  , "react-basic-hooks"
  , "homotopy-core"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
