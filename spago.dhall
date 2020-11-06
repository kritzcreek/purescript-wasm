{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "wasm"
, dependencies =
    [ "console"
    , "effect"
    , "nullable"
    , "exceptions"
    , "refs"
    , "integers"
    , "arrays"
    , "dynamic-buffer"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
