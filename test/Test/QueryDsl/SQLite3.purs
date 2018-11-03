module Test.QueryDsl.SQLite3 (test) where

import Prelude

import Effect.Aff (bracket)
import QueryDsl (Column, SelectQuery, Table, columns, deleteFrom, from, insertInto, makeTable, select, update)
import QueryDsl.Expressions ((:+), (:==), sum)
import QueryDsl.SQLite3 (runQuery, runSelectOneQuery)
import SQLite3 as SQLite3
import Test.QueryDsl.SQLite3.Expressions as Expressions
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Data.Boolean (False, True)

createTableSql :: String
createTableSql = """
create table test (
  id integer not null primary key autoincrement,
  name text not null,
  count int not null
)
"""

testTable = makeTable "test" :: Table (
  id :: Column Int False,
  name :: Column String True,
  count :: Column Int True
)

selectCount :: SelectQuery (n :: Int)
selectCount = do
  t <- from testTable
  pure $ select {n: sum t.count}

test :: Spec Unit
test = do
  it "Sqlite" do
    bracket (SQLite3.newDB ":memory:") SQLite3.closeDB \conn -> do

      void $ SQLite3.queryDB conn createTableSql []

      let t = columns testTable

      runQuery conn $ insertInto testTable {name: "jim", count: 5}
      runQuery conn $ insertInto testTable {name: "jane", count: 42}
      runQuery conn $ insertInto testTable {name: "jean", count: 7}

      count <- runSelectOneQuery conn selectCount
      count.n `shouldEqual` 54

      runQuery conn $ update testTable {count: t.count :+ 1} (t.name :== "jim")

      count' <- runSelectOneQuery conn selectCount
      count'.n `shouldEqual` 55

      runQuery conn $ deleteFrom testTable (t.name :== "jean")

      count'' <- runSelectOneQuery conn selectCount
      count''.n `shouldEqual` 48

  Expressions.test
