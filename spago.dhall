{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "wasm"
, dependencies =
  [ "arrays"
  , "console"
  , "dynamic-buffer"
  , "effect"
  , "exceptions"
  , "integers"
  , "nullable"
  , "ordered-collections"
  , "record"
  , "refs"
  , "transformers"
  , "parsing"
  , "generics-rep"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
