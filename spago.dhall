{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "psci-support"
    , "nullable"
    , "exceptions"
    , "arraybuffer-types"
    , "refs"
    , "integers"
    , "arrays"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
