-- | Some operators for building where clauses and other expressions.
-- |
-- | The operator precedence used here is designed to match the normal purescript operators.
module QueryDsl.Expressions (
  eq,
  (:==),
  ne,
  (:/=),
  and,
  (:&&),
  or,
  (:||),
  not,
  plus,
  (:+),
  minus,
  (:-),
  multiply,
  (:*),
  divide,
  (:/),
  negate,
  lt,
  (:<),
  le,
  (:<=),
  gt,
  (:>),
  ge,
  (:>=),
  is,
  isNot,
  isNull,
  isNotNull,
  avg,
  avgDistinct,
  count,
  countDistinct,
  countAll,
  min,
  max,
  sum,
  sumDistinct) where

import QueryDsl (Expression, BinaryOperator, UnaryOperator, binaryOperator, postfixOperator, prefixOperator, unaryAggregateFunction, nullaryFunction)

eq :: forall a b c. BinaryOperator a b c Boolean
eq = binaryOperator "="

infixl 4 eq as :==

ne :: forall a b c. BinaryOperator a b c Boolean
ne = binaryOperator "<>"

infixl 4 ne as :/=

and :: forall a b. BinaryOperator a b Boolean Boolean
and = binaryOperator "and"

infixl 3 and as :&&

or :: forall a b. BinaryOperator a b Boolean Boolean
or = binaryOperator "or"

infixl 2 or as :||

not :: forall a. UnaryOperator a Boolean Boolean
not = prefixOperator "not"

plus :: forall a b c. BinaryOperator a b c c
plus = binaryOperator "+"

infixl 6 plus as :+

minus :: forall a b c. BinaryOperator a b c c
minus = binaryOperator "-"

infixl 6 minus as :-

multiply :: forall a b c. BinaryOperator a b c c
multiply = binaryOperator "*"

infixl 7 multiply as :*

divide :: forall a b c. BinaryOperator a b c c
divide = binaryOperator "/"

infixl 7 divide as :/

negate :: forall a b. UnaryOperator a b b
negate = prefixOperator "-"

lt :: forall a b c. BinaryOperator a b c Boolean
lt = binaryOperator "<"

infixl 4 lt as :<

le :: forall a b c. BinaryOperator a b c Boolean
le = binaryOperator "<="

infixl 4 le as :<=

gt :: forall a b c. BinaryOperator a b c Boolean
gt = binaryOperator ">"

infixl 4 gt as :>

ge :: forall a b c. BinaryOperator a b c Boolean
ge = binaryOperator ">="

infixl 4 ge as :>=

is :: forall a b c. BinaryOperator a b c Boolean
is = binaryOperator "is"

isNot :: forall a b c. BinaryOperator a b c Boolean
isNot = binaryOperator "is not"

isNull :: forall a b. UnaryOperator a b Boolean
isNull = postfixOperator "is null"

isNotNull :: forall a b. UnaryOperator a b Boolean
isNotNull = postfixOperator "is not null"

avg :: forall a b. UnaryOperator a b Number
avg = unaryAggregateFunction "avg" false

avgDistinct :: forall a b. UnaryOperator a b Number
avgDistinct = unaryAggregateFunction "avg" true

count :: forall a b. UnaryOperator a b Int
count = unaryAggregateFunction "count" false

countDistinct :: forall a b. UnaryOperator a b Int
countDistinct = unaryAggregateFunction "count" true

countAll :: Expression Int
countAll = nullaryFunction "count(*)"

min :: forall a b. UnaryOperator a b b
min = unaryAggregateFunction "min" false

max :: forall a b. UnaryOperator a b b
max = unaryAggregateFunction "max" false

sum :: forall a b. UnaryOperator a b b
sum = unaryAggregateFunction "sum" false

sumDistinct :: forall a b. UnaryOperator a b b
sumDistinct = unaryAggregateFunction "sum" true
