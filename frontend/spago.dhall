{ name = "halogen-project"
, dependencies = [ 
    "affjax",
    "argonaut-codecs",
    "console", 
    "effect", 
    "halogen",
    "psci-support",
    "foreign-generic",
    "random"
   ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
