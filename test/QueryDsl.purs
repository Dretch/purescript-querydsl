module Test.QueryDsl (test) where

import Prelude

import Data.Maybe (Maybe(..))
import QueryDsl
import QueryDsl.Expressions ((:==), (:+), (:*))
import Test.QueryDsl.Expressions as Expressions
import Test.Spec (Spec, describeOnly, it)
import Test.Spec.Assertions (shouldEqual)

c :: forall t. SqlType t => t -> Constant
c = toConstant

testTable :: Table _
testTable = makeTable "test"
  `addColumn` (column :: Column "id" String)
  `addColumn` (column :: Column "count" Int)
  `addColumn` (column :: Column "description" (Maybe String))

simpleSelectQuery :: SelectQuery (id :: String, count :: Int)
simpleSelectQuery =
  let t = from testTable in
  selectFrom testTable {id: t.id, count: t.count} alwaysTrue

filteredSelectQuery :: SelectQuery (id :: String)
filteredSelectQuery =
  let t = from testTable in
  selectFrom testTable {id: t.id} (t.id :== "abc")

expressiveSelectQuery :: SelectQuery (id :: String, count :: Int)
expressiveSelectQuery =
  let t = from testTable in
  selectFrom testTable {id: t.id, count: t.count :+ 1} alwaysTrue

filteredUpdateQuery :: UpdateQuery
filteredUpdateQuery =
  let t = from testTable in
  update testTable {count: t.count :* 2} (t.id :== "abc")

filteredDeleteQuery :: DeleteQuery
filteredDeleteQuery =
  let t = from testTable in
  deleteFrom testTable (t.id :== "abc")

insertQuery :: InsertQuery
insertQuery = insertInto testTable {id: "abc", count: 123, description: Nothing :: Maybe String}

test :: Spec Unit
test = do
  describeOnly "QueryDsl" do

    it "createTableSql" do
      createTableSql testTable `shouldEqual` ParameterizedSql
        "create table test (id text not null, count integer not null, description text)" []

    it "simpleSelectQuery" do
      selectSql simpleSelectQuery `shouldEqual` ParameterizedSql
        "select test.count, test.id from test" []

    it "filteredSelectQuery" do
      selectSql filteredSelectQuery `shouldEqual` ParameterizedSql
        "select test.id from test where (test.id = ?)" [ c "abc" ]

    it "expressiveSelectQuery" do
      selectSql expressiveSelectQuery `shouldEqual` ParameterizedSql
        "select (test.count + ?) as count, test.id from test" [ c 1 ]

    it "filteredDeleteQuery" do
      deleteSql filteredDeleteQuery `shouldEqual` ParameterizedSql
        "delete from test where (test.id = ?)" [ c "abc" ]

    it "insertQuery" do
      insertSql insertQuery `shouldEqual` ParameterizedSql
        "insert into test (id, description, count) values (?, ?, ?)"
        [ c "abc", NullConstant, c 123 ]

    it "filteredUpdateQuery" do
      updateSql filteredUpdateQuery `shouldEqual` ParameterizedSql
        "update test set count = (test.count * ?) where (test.id = ?)"
        [ c 2, c "abc" ]

    Expressions.test
