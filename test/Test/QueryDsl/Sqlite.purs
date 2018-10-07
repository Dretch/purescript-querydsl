module Test.QueryDsl.Sqlite (test) where

import Prelude

import Effect.Class (liftEffect)
import QueryDsl (Column, SelectQuery, Table, alwaysTrue, columns, deleteFrom, from, insertInto, makeTable, select, update)
import QueryDsl.Expressions ((:+), (:==), sum)
import QueryDsl.SQLite (runQuery, runSelectOneQuery)
import SQLite3 as SQLite3
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

selectCount :: SelectQuery (n :: Int) Unit
selectCount = do
  t <- from testTable
  select {n: sum t.count} alwaysTrue

test :: Spec Unit
test = do
  it "Sqlite" do

    conn <- SQLite3.newDB ":memory:"
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

    liftEffect $ SQLite3.closeDB conn
