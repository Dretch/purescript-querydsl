module Test.QueryDsl (test) where

import Prelude hiding (join)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import QueryDsl
import QueryDsl.Expressions ((:==), (:/=), (:+), (:*))
import Test.QueryDsl.Assertions (shouldBeSql)
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

testChildTable :: Table _
testChildTable = makeTable "child"
  `addColumn` (column :: Column "id" String)
  `addColumn` (column :: Column "extra" String)

simpleSelectQuery :: SelectQuery (id :: String, count :: Int) Unit
simpleSelectQuery = do
  t <- from testTable
  select {id: t.id, count: t.count} alwaysTrue

filteredSelectQuery :: SelectQuery (id :: String) Unit
filteredSelectQuery = do
  t <- from testTable
  select {id: t.id} (t.id :== "abc")

expressiveSelectQuery :: SelectQuery (id :: String, count :: Int) Unit
expressiveSelectQuery = do
  t <- from testTable
  select {id: t.id, count: t.count :+ 1} alwaysTrue

simpleJoinSelectQuery :: SelectQuery (jId :: String, tId :: String, extra :: String) Unit
simpleJoinSelectQuery = do
  t <- from testTable
  j <- join testChildTable (\j -> j.id :== t.id)
  select {tId: t.id, jId: j.id, extra: j.extra} (t.id :/= "sdf")

selectQueryWithNoFrom :: SelectQuery (id :: String) Unit
selectQueryWithNoFrom =
  let t = columns testTable in
  select {id: t.id} alwaysTrue

selectQueryWithNoSelect :: SelectQuery (id :: String) Unit
selectQueryWithNoSelect =
  void $ from testTable

selfJoinSelectQuery :: SelectQuery (aId :: String, bId ::String) Unit
selfJoinSelectQuery = do
  a <- from testTable
  b <- join testTable (\b -> b.id :== a.id)
  select {aId: a.id, bId: b.id} alwaysTrue

filteredUpdateQuery :: UpdateQuery
filteredUpdateQuery =
  let t = columns testTable in
  update testTable {count: t.count :* 2} (t.id :== "abc")

filteredDeleteQuery :: DeleteQuery
filteredDeleteQuery =
  let t = columns testTable in
  deleteFrom testTable (t.id :== "abc")

insertQuery :: InsertQuery
insertQuery = insertInto testTable {id: "abc", count: 123, description: Nothing :: Maybe String}

test :: Spec Unit
test = do
  describeOnly "QueryDsl" do

    it "simpleSelectQuery" do
      selectSql simpleSelectQuery `shouldBeSql` ParameterizedSql
        "select a.count, a.id from test as a" []

    it "filteredSelectQuery" do
      selectSql filteredSelectQuery `shouldBeSql` ParameterizedSql
        "select a.id from test as a where (a.id = ?)" [ c "abc" ]

    it "expressiveSelectQuery" do
      selectSql expressiveSelectQuery `shouldBeSql` ParameterizedSql
        "select (a.count + ?) as count, a.id from test as a" [ c 1 ]

    it "simpleJoinSelectQuery" do
      selectSql simpleJoinSelectQuery `shouldBeSql` ParameterizedSql
        "select b.extra, b.id as jId, a.id as tId from test as a join child as b on (b.id = a.id) where (a.id <> ?)"
        [ c "sdf" ]

    it "selfJoinSelectQuery" do
      selectSql selfJoinSelectQuery `shouldBeSql` ParameterizedSql
        "select a.id as aId, b.id as bId from test as a join test as b on (b.id = a.id)" []

    it "selectQueryWithNoFrom" do
      selectSql selectQueryWithNoFrom `shouldEqual` Left "SQL query is missing initial from-clause"

    it "selectQueryWithNoSelect" do
      selectSql selectQueryWithNoSelect `shouldEqual` Left "SQL query is missing initial from-clause"

    it "filteredDeleteQuery" do
      deleteSql filteredDeleteQuery `shouldBeSql` ParameterizedSql
        "delete from test where (test.id = ?)" [ c "abc" ]

    it "insertQuery" do
      insertSql insertQuery `shouldBeSql` ParameterizedSql
        "insert into test (id, description, count) values (?, ?, ?)"
        [ c "abc", NullConstant, c 123 ]

    it "filteredUpdateQuery" do
      updateSql filteredUpdateQuery `shouldBeSql` ParameterizedSql
        "update test set count = (test.count * ?) where (test.id = ?)"
        [ c 2, c "abc" ]

    Expressions.test
