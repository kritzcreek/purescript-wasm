{ name = "wasm-example"
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
, sources = [ "src/**/*.purs", "example/**/*.purs" ]
}
