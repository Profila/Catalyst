{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "profila"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "bigints"
  , "cardano-transaction-lib"
  , "lists"
  , "monad-logger"
  , "ordered-collections"
  , "prelude"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
