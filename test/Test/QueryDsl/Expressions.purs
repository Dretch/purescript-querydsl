module Test.QueryDsl.Expressions (test) where

import QueryDsl (class SqlType, Constant(..), ParameterizedSql(..), expressionSql, toConstant)
import QueryDsl.Expressions (is, isNot, isNotNull, isNull, negate, not, (:&&), (:*), (:+), (:-), (:/), (:/=), (:<), (:<=), (:==), (:>), (:>=), (:||))

import Data.Maybe (Maybe(..))
import Prelude (Unit, discard)
import Test.QueryDsl.Assertions (shouldBeSql)
import Test.Spec (Spec, describe, it)

c :: forall t. SqlType t => t -> Constant
c = toConstant

test :: Spec Unit
test = do
  describe "Expressions" do

    it "equality" do
      expressionSql (1 :== 1 :&& 2 :/= 3) `shouldBeSql` ParameterizedSql
        "((? = ?) and (? <> ?))" [ c 1, c 1, c 2, c 3 ]

    it "null-safe equality" do
      let sql = expressionSql (Just 1 `is` Nothing :&& Nothing `isNot` Just 1)
      sql `shouldBeSql` ParameterizedSql
        "((? is ?) and (? is not ?))" [ c 1, NullConstant, NullConstant, c 1]

    it "nullability" do
      let sql = expressionSql (isNull 4 :|| isNotNull false)
      sql `shouldBeSql` ParameterizedSql
        "((? is null) or (? is not null))" [ c 4, c 0 ]

    it "booleans" do
      expressionSql (true :|| false :&& true) `shouldBeSql` ParameterizedSql
        "(? or (? and ?))" [ c 1, c 0, c 1 ]
      expressionSql (not true) `shouldBeSql` ParameterizedSql
        "(not ?)" [ c 1 ]

    it "basic arithmetic" do
      let sql = expressionSql (1.0 :+ 3.0 :* 4.0 :- 3.0 :/ 2.0)
      sql `shouldBeSql` ParameterizedSql
        "((? + (? * ?)) - (? / ?))"
        [ c 1.0, c 3.0, c 4.0, c 3.0, c 2.0 ]

    it "negation" do
      expressionSql (negate 12) `shouldBeSql` ParameterizedSql
        "(- ?)" [ c 12 ]

    it "orderings" do
      let sql = expressionSql (1 :< 2 :&& 2 :<= 3 :&& 4 :> 2 :&& 4 :>= 4)
      sql `shouldBeSql` ParameterizedSql
        "((((? < ?) and (? <= ?)) and (? > ?)) and (? >= ?))"
        [ c 1, c 2, c 2, c 3, c 4, c 2, c 4, c 4 ]
