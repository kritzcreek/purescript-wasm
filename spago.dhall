{ name = "wasm-example"
, dependencies =
  [ "arraybuffer-types"
  , "arrays"
  , "console"
  , "control"
  , "dodo-printer"
  , "dynamic-buffer"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "identity"
  , "integers"
  , "maybe"
  , "nullable"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "record"
  , "refs"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "example/**/*.purs" ]
}
