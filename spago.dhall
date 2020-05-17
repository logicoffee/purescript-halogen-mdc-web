{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-halogen-mdc-web"
, dependencies =
  [ "console"
  , "css"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "halogen-vdom"
  , "psci-support"
  , "strings"
  , "unsafe-coerce"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
