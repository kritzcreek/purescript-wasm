{ name = "wasm"
, dependencies =
  [ "arraybuffer-types"
  , "arrays"
  , "dynamic-buffer"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "partial"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
