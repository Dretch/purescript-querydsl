module Test.QueryDsl (test) where

import QueryDsl

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Prelude (Unit, bind, discard, pure, ($))
import QueryDsl.Expressions ((:==), (:/=), (:+), (:*))
import Test.QueryDsl.Assertions (shouldBeSql)
import Test.QueryDsl.Expressions as Expressions
import Test.QueryDsl.Sqlite as Sqlite
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Data.Boolean (False, True)
import Type.Row (RProxy(..))

c :: forall t. SqlType t => t -> Constant
c = toConstant

testTable = makeTable "test" :: Table (
  id :: Column String False,
  count :: Column Int True,
  description :: Column (Maybe String) False
)

testChildTable = makeTable "child" :: Table (
  id :: Column String True,
  extra :: Column String True
)

simpleSelectQuery :: SelectQuery (id :: String, count :: Int)
simpleSelectQuery = do
  t <- from testTable
  pure $ select {id: t.id, count: t.count}

filteredSelectQuery :: SelectQuery (id :: String)
filteredSelectQuery = do
  t <- from testTable
  pure $ select {id: t.id} `where_` (t.id :== "abc")

selectQueryWithLimit :: SelectQuery (id :: String)
selectQueryWithLimit = do
  t <- from testTable
  pure $ select {id: t.id} `limit` 10

selectQueryWithOffset :: SelectQuery (id :: String)
selectQueryWithOffset = do
  t <- from testTable
  pure $ select {id: t.id} `offset` 50

selectQueryWithLimitAndOffset :: SelectQuery (id :: String)
selectQueryWithLimitAndOffset = do
  t <- from testTable
  pure $ select {id: t.id} `limit` 10 `offset` 50

expressiveSelectQuery :: SelectQuery (id :: String, count :: Int)
expressiveSelectQuery = do
  t <- from testTable
  pure $ select {id: t.id, count: t.count :+ 1}

simpleJoinSelectQuery :: SelectQuery (jId :: String, tId :: String, extra :: String)
simpleJoinSelectQuery = do
  t <- from testTable
  j <- join testChildTable (\j -> j.id :== t.id)
  pure $ select {tId: t.id, jId: j.id, extra: j.extra} `where_` (t.id :/= "sdf")

selectQueryWithOrderBy :: SelectQuery (id :: String)
selectQueryWithOrderBy = do
  t <- from testTable
  pure $ select {id: t.id} `orderBy` [asc t.description, desc t.id]

selectQueryWithNoFrom :: SelectQuery (id :: String)
selectQueryWithNoFrom =
  let t = columns testTable in
  pure $ select {id: t.id}

selfJoinSelectQuery :: SelectQuery (aId :: String, bId ::String)
selfJoinSelectQuery = do
  a <- from testTable
  b <- join testTable (\b -> b.id :== a.id)
  pure $ select {aId: a.id, bId: b.id}

filteredUpdateQuery :: UpdateQuery
filteredUpdateQuery =
  let t = columns testTable in
  update testTable {count: t.count :* 2} (t.id :== "abc")

filteredDeleteQuery :: DeleteQuery
filteredDeleteQuery =
  let t = columns testTable in
  deleteFrom testTable (t.id :== "abc")

insertQuery :: InsertQuery
insertQuery =
  insertInto testTable {id: "abc", count: 123, description: Nothing :: Maybe String}

insertQueryWithRequiredFieldsOnly :: InsertQuery
insertQueryWithRequiredFieldsOnly =
  insertInto testTable {count: 123}

sqlType :: Spec Unit
sqlType = do
  describe "SqlType" do

    describe "String" do
      it "toConstant" do
        toConstant "abc" `shouldEqual` StringConstant "abc"
      it "fromConstant" do
        fromConstant (StringConstant "abc") `shouldEqual` Just "abc"
        fromConstant (IntConstant 123) `shouldEqual` Nothing :: Maybe String
        fromConstant (NumberConstant 123.0) `shouldEqual` Nothing :: Maybe String
        fromConstant NullConstant `shouldEqual` Nothing :: Maybe String

    describe "Int" do
      it "toConstant" do
        toConstant 123 `shouldEqual` IntConstant 123
      it "fromConstant" do
        fromConstant (IntConstant 123) `shouldEqual` Just 123
        fromConstant (StringConstant "abc") `shouldEqual` Nothing :: Maybe Int
        fromConstant (NumberConstant 123.0) `shouldEqual` Nothing :: Maybe Int
        fromConstant NullConstant `shouldEqual` Nothing :: Maybe Int

    describe "Number" do
      it "toConstant" do
        toConstant 123.456 `shouldEqual` NumberConstant 123.456
      it "fromConstant" do
        fromConstant (NumberConstant 123.0) `shouldEqual` Just 123.0
        fromConstant (IntConstant 123) `shouldEqual` Just 123.0
        fromConstant (StringConstant "abc") `shouldEqual` Nothing :: Maybe Number
        fromConstant NullConstant `shouldEqual` Nothing :: Maybe Number

    describe "Boolean" do
      it "toConstant" do
        toConstant false `shouldEqual` IntConstant 0
        toConstant true `shouldEqual` IntConstant 1
      it "fromConstant" do
        fromConstant (NumberConstant 123.0) `shouldEqual` Nothing :: Maybe Boolean
        fromConstant (StringConstant "abc") `shouldEqual` Nothing :: Maybe Boolean
        fromConstant (IntConstant 0) `shouldEqual` Just false
        fromConstant (IntConstant 1) `shouldEqual` Just true
        fromConstant (IntConstant 42) `shouldEqual` Just true
        fromConstant NullConstant `shouldEqual` Nothing :: Maybe Boolean

    describe "Maybe" do
      it "toConstant" do
        toConstant (Nothing :: Maybe Int) `shouldEqual` NullConstant
        toConstant (Just 1) `shouldEqual` IntConstant 1
      it "fromConstant" do
        fromConstant NullConstant `shouldEqual` Just (Nothing :: Maybe Int)
        fromConstant (IntConstant 123) `shouldEqual` Just (Just 123)
        fromConstant (StringConstant "abc") `shouldEqual` Nothing :: Maybe Int

sqlGeneration :: Spec Unit
sqlGeneration = do
  describe "toSql" do

    it "simpleSelectQuery" do
      toSql simpleSelectQuery `shouldBeSql` ParameterizedSql
        "select a.count, a.id from test as a" []

    it "filteredSelectQuery" do
      toSql filteredSelectQuery `shouldBeSql` ParameterizedSql
        "select a.id from test as a where (a.id = ?)" [ c "abc" ]

    it "expressiveSelectQuery" do
      toSql expressiveSelectQuery `shouldBeSql` ParameterizedSql
        "select (a.count + ?) as count, a.id from test as a" [ c 1 ]

    it "simpleJoinSelectQuery" do
      toSql simpleJoinSelectQuery `shouldBeSql` ParameterizedSql
        "select b.extra, b.id as jId, a.id as tId from test as a join child as b on (b.id = a.id) where (a.id <> ?)"
        [ c "sdf" ]

    it "selfJoinSelectQuery" do
      toSql selfJoinSelectQuery `shouldBeSql` ParameterizedSql
        "select a.id as aId, b.id as bId from test as a join test as b on (b.id = a.id)" []

    it "selectQueryWithOrderBy" do
      toSql selectQueryWithOrderBy `shouldBeSql` ParameterizedSql
        "select a.id from test as a order by a.description asc, a.id desc" []

    it "selectQueryWithLimit" do
      toSql selectQueryWithLimit `shouldBeSql` ParameterizedSql
        "select a.id from test as a limit ?" [c 10]

    it "selectQueryWithOffset" do
      toSql selectQueryWithOffset `shouldBeSql` ParameterizedSql
        "select a.id from test as a limit -1 offset ?" [c 50]

    it "selectQueryWithLimitAndOffset" do
      toSql selectQueryWithLimitAndOffset `shouldBeSql` ParameterizedSql
        "select a.id from test as a limit ? offset ?" [c 10, c 50]

    it "selectQueryWithNoFrom" do
      toSql selectQueryWithNoFrom `shouldEqual` Left "SQL query is missing initial table"

    it "filteredDeleteQuery" do
      toSql filteredDeleteQuery `shouldBeSql` ParameterizedSql
        "delete from test where (test.id = ?)" [ c "abc" ]

    it "insertQuery" do
      toSql insertQuery `shouldBeSql` ParameterizedSql
        "insert into test (id, description, count) values (?, ?, ?)"
        [ c "abc", NullConstant, c 123 ]

    it "insertQueryWithRequiredFieldsOnly" do
      toSql insertQueryWithRequiredFieldsOnly `shouldBeSql` ParameterizedSql
        "insert into test (count) values (?)"
        [ c 123 ]

    it "filteredUpdateQuery" do
      toSql filteredUpdateQuery `shouldBeSql` ParameterizedSql
        "update test set count = (test.count * ?) where (test.id = ?)"
        [ c 2, c "abc" ]

resultGeneration :: Spec Unit
resultGeneration = do
  describe "constantsToRecord" do

    let idProxy = RProxy :: RProxy (id :: Int)
    let idNameProxy = RProxy :: RProxy (id :: Int, name :: String)

    it "fields match" do
      constantsToRecord idNameProxy (Map.insert "id" (c 123) $ Map.singleton "name" (c "abc"))
        `shouldEqual` Right {id: 123, name: "abc"}

    it "extra field value supplied" do
      constantsToRecord idProxy (Map.insert "id" (c 123) $ Map.singleton "extra" (c 456))
        `shouldEqual` Left "Value supplied for unknown field: extra = 456"

    it "expected field value missing" do
      constantsToRecord idNameProxy (Map.singleton "id" (c 123))
        `shouldEqual` Left "No value found for required field: name"

    it "field value with incorrect type" do
      constantsToRecord idProxy (Map.singleton "id" (c "123"))
        `shouldEqual` Left "Value has incorrect type for field id, unable to convert: \"123\""

test :: Spec Unit
test = do
  describe "QueryDsl" do
    sqlGeneration
    resultGeneration
    sqlType
    Expressions.test
    Sqlite.test
