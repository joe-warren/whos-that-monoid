{ name = "halogen-project"
, dependencies = [ 
    "affjax",
  "argonaut-codecs",
    "console", 
    "effect", 
    "halogen",
     "psci-support",
     "foreign-generic"
     ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
