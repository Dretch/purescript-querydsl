module Test.QueryDsl.Expressions (test) where

import Data.Maybe (Maybe(..))
import Prelude (Unit, discard)
import QueryDsl (expressionSql)
import QueryDsl.Expressions
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

test :: Spec Unit
test = do
  describe "Expressions" do

    it "equality" do
      expressionSql (1 :== 1 :&& 2 :/= 3) `shouldEqual` "((1 = 1) and (2 <> 3))"

    it "null-safe equality" do
      let sql = expressionSql (Just 1 `is` Nothing :&& Nothing `isNot` Just 1)
      sql `shouldEqual` "((1 is null) and (null is not 1))"

    it "nullability" do
      let sql = expressionSql (isNull 4 :|| isNotNull false)
      sql `shouldEqual` "((4 is null) or (0 is not null))"

    it "booleans" do
      expressionSql (true :|| false :&& true) `shouldEqual` "(1 or (0 and 1))"
      expressionSql (not true) `shouldEqual` "(not 1)"

    it "basic arithmetic" do
      let sql = expressionSql (1.0 :+ 3.0 :* 4.0 :- 3.0 :/ 2.0)
      sql `shouldEqual` "((1.0 + (3.0 * 4.0)) - (3.0 / 2.0))"

    it "negation" do
      expressionSql (negate 12) `shouldEqual` "(- 12)"

    it "orderings" do
      let sql = expressionSql (1 :< 2 :&& 2 :<= 3 :&& 4 :> 2 :&& 4 :>= 4)
      sql `shouldEqual` "((((1 < 2) and (2 <= 3)) and (4 > 2)) and (4 >= 4))"
