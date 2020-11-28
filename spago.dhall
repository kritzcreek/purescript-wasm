{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "wasm"
, dependencies =
  [ "arrays"
  , "console"
  , "dodo-printer"
  , "dynamic-buffer"
  , "effect"
  , "exceptions"
  , "generics-rep"
  , "integers"
  , "nullable"
  , "ordered-collections"
  , "parsing"
  , "record"
  , "refs"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
