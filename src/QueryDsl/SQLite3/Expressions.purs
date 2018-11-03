-- | SQLite-specific functions and operators.
module QueryDsl.SQLite3.Expressions (random) where

import QueryDsl (Expression, nullaryFunction)

random :: Expression Int
random = nullaryFunction "random()"
