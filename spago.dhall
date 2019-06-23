{ name =
    "querydsl"
, dependencies =
    [ "prelude"
    , "effect"
    , "arrays"
    , "lists"
    , "record"
    , "strings"
    , "tuples"
    , "typelevel-prelude"
    , "either"
    , "transformers"
    , "node-sqlite3"
    , "datetime"
    , "formatters"
    , "node-buffer"
    , "nullable"
    , "spec"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
, license =
    "WTFPL"
, repository =
    "https://github.com/Dretch/purescript-querydsl.git"
}
