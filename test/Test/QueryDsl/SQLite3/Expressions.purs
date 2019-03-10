module Test.QueryDsl.SQLite3.Expressions (test) where

import Prelude

import QueryDsl (class SqlType, Constant, ParameterizedSql(..), expressionSql, toConstant)
import QueryDsl.SQLite3.Expressions (random, match)
import Test.QueryDsl.Assertions (shouldBeSql)
import Test.Spec (Spec, describe, it)

c :: forall t. SqlType t => t -> Constant
c = toConstant

test :: Spec Unit
test = do
  describe "Expressions" do

    it "random" do
      expressionSql random `shouldBeSql` ParameterizedSql "random()" []

    it "match" do
      expressionSql ("abc" `match` "a") `shouldBeSql` ParameterizedSql "(? match ?)"
        [ c "abc", c "a" ]
