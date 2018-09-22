module Test.QueryDsl (test) where

import Prelude (Unit, discard)
import QueryDsl (Column, InsertQuery, DeleteQuery, SelectQuery, Table, addColumn, column, createTableSql, deleteFrom, deleteSql, filter, from, insertInto, insertSql, makeTable, select, selectSql, (++))
import QueryDsl.Expressions ((:==))
import Test.QueryDsl.Expressions as Expressions
import Test.Spec (Spec, describeOnly, it)
import Test.Spec.Assertions (shouldEqual)

-- TODO: nullable columns?
testTable :: Table _
testTable = makeTable "test"
  `addColumn` (column :: Column "id" String)
  `addColumn` (column :: Column "count" Int)

simpleSelectQuery :: SelectQuery (id :: String, count :: Int)
simpleSelectQuery =
  let t = from testTable in
  select t.id ++ t.count

filteredSelectQuery :: SelectQuery (id :: String)
filteredSelectQuery =
  let t = from testTable in
  select t.id `filter` (t.id :== "abc")

filteredDeleteQuery :: DeleteQuery
filteredDeleteQuery =
  let t = from testTable in
  deleteFrom testTable (t.id :== "abc")

insertQuery :: InsertQuery
insertQuery = insertInto testTable {id: "abc", count: 123}

test :: Spec Unit
test = do
  describeOnly "QueryDsl" do

    it "createTableSql" do
      createTableSql testTable `shouldEqual` "create table test (id text not null, count integer not null)"

    it "simpleSelectQuery" do
      selectSql simpleSelectQuery `shouldEqual` "select test.id, test.count from test"

    it "filteredSelectQuery" do
      selectSql filteredSelectQuery `shouldEqual` "select test.id from test where (test.id = 'abc')"

    it "filteredDeleteQuery" do
      deleteSql filteredDeleteQuery `shouldEqual` "delete from test where (test.id = 'abc')"

    it "insertQuery" do
      insertSql insertQuery `shouldEqual` "insert into test (id, count) values ('abc', 123)"

    Expressions.test
