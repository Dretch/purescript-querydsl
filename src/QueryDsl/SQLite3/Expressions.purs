-- | SQLite-specific functions and operators.
module QueryDsl.SQLite3.Expressions
  ( random
  , rank
  , match ) where

import QueryDsl (BinaryOperator, Expression, binaryOperator, nullaryFunction)

random :: Expression Int
random = nullaryFunction "random()"

rank :: Expression Number
rank = nullaryFunction "rank"

match :: forall a b. BinaryOperator a b String Boolean
match = binaryOperator "match"
