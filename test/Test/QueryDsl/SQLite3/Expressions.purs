module Test.QueryDsl.SQLite3.Expressions (test) where

import Prelude (Unit)
import QueryDsl (ParameterizedSql(..), expressionSql)
import QueryDsl.SQLite3.Expressions (random)
import Test.QueryDsl.Assertions (shouldBeSql)
import Test.Spec (Spec, describe, it)

test :: Spec Unit
test = do
  describe "Expressions" do

    it "random" do
      expressionSql random `shouldBeSql` ParameterizedSql "random()" []
