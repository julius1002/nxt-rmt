{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "console"
  , "effect"
  , "either"
  , "halogen"
  , "maybe"
  , "prelude"
  , "read"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
