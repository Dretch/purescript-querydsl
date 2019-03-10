-- | SQLite-specific functions and operators.
module QueryDsl.SQLite3.Expressions
  ( random
  , match ) where

import QueryDsl (BinaryOperator, Expression, binaryOperator, nullaryFunction)

random :: Expression Int
random = nullaryFunction "random()"

match :: forall a b. BinaryOperator a b String Boolean
match = binaryOperator "match"
