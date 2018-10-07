module Test.QueryDsl.Sqlite (test) where

import Prelude

import Data.Foldable (sum)
import Effect.Class (liftEffect)
import QueryDsl (Column, SelectQuery, Table, alwaysTrue, columns, deleteFrom, from, insertInto, makeTable, select, update)
import QueryDsl.Expressions ((:+), (:==))
import QueryDsl.SQLite (runQuery, runSelectManyQuery)
import SQLite3 as SQLite3
import Test.Spec (Spec, describeOnly, it)
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

-- todo: use aggreate function to sum!
selectCounts :: SelectQuery (count :: Int) Unit
selectCounts = do
  t <- from testTable
  select {count: t.count} alwaysTrue

test :: Spec Unit
test = do
  describeOnly "Sqlite" do

    it "test" do
      conn <- SQLite3.newDB ":memory:"
      _ <- SQLite3.queryDB conn createTableSql []

      runQuery conn $ insertInto testTable {name: "jim", count: 5}
      runQuery conn $ insertInto testTable {name: "jane", count: 42}
      runQuery conn $ insertInto testTable {name: "jean", count: 7}

      counts <- runSelectManyQuery conn selectCounts
      sum (_.count <$> counts) `shouldEqual` 54

      runQuery conn $
        let t = columns testTable in
        update testTable {count: t.count :+ 1} (t.name :== "jim")

      counts' <- runSelectManyQuery conn selectCounts
      sum (_.count <$> counts') `shouldEqual` 55

      runQuery conn $
        let t = columns testTable in
        deleteFrom testTable (t.name :== "jean")

      counts'' <- runSelectManyQuery conn selectCounts
      sum (_.count <$> counts'') `shouldEqual` 48

      liftEffect $ SQLite3.closeDB conn
