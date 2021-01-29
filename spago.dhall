{ name = "wasm"
, dependencies =
  [ "arrays"
  , "dynamic-buffer"
  , "effect"
  , "integers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
