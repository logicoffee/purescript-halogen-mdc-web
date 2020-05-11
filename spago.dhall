{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-halogen-mdc-web"
, dependencies =
  [ "console", "effect", "halogen", "psci-support", "web-uievents" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
