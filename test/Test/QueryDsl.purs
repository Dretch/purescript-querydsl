module Test.QueryDsl (test) where

import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Time.Duration (Milliseconds(..))
import Node.Buffer (Octet)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, pure, ($), negate, (==))
import QueryDsl
import QueryDsl.Expressions (countAll, (:*), (:+), (:==), (:>=))
import Test.QueryDsl.Assertions (shouldBeSql)
import Test.QueryDsl.Expressions as Expressions
import Test.QueryDsl.SQLite3 as SQLite3
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
  j <- innerJoin testChildTable (\j -> j.id :== t.id)
  pure $ select {tId: t.id, jId: j.id, extra: j.extra}

leftJoinSelectQuery :: SelectQuery (jId :: String, tId :: String, extra :: String)
leftJoinSelectQuery = do
  t <- from testTable
  j <- leftJoin testChildTable (\j -> j.id :== t.id)
  pure $ select {tId: t.id, jId: j.id, extra: j.extra}

crossJoinSelectQuery :: SelectQuery (jId :: String, tId :: String, extra :: String)
crossJoinSelectQuery = do
  t <- from testTable
  j <- crossJoin testChildTable
  pure $ select {tId: t.id, jId: j.id, extra: j.extra}

selectQueryWithOrderBy :: SelectQuery (id :: String)
selectQueryWithOrderBy = do
  t <- from testTable
  pure $ select {id: t.id} `orderBy` [asc t.description, desc t.id]

selectQueryWithGroupBy :: SelectQuery (id :: String, n :: Int)
selectQueryWithGroupBy = do
  t <- from testTable
  pure $ select {id: t.id, n: countAll} `groupBy` t.description

selectQueryWithTwoGroupBys :: SelectQuery (id :: String, n :: Int)
selectQueryWithTwoGroupBys = do
  t <- from testTable
  pure $ select {id: t.id, n: countAll} `groupBy` t.description `groupBy` t.id

selectQueryWithGroupByHaving :: SelectQuery (id :: String, n :: Int)
selectQueryWithGroupByHaving = do
  t <- from testTable
  pure $ select {id: t.id, n: countAll} `groupBy` t.description `having` (countAll :>= 5)

selectQueryWithNoFrom :: SelectQuery (int :: Int, string :: String)
selectQueryWithNoFrom =
  pure $ select {int: 123, string: "abc"}

selectQueryWithFirstTableAsJoin :: SelectQuery (id :: String)
selectQueryWithFirstTableAsJoin = do
  t <- innerJoin testTable (\t -> t.id :== "abc")
  pure $ select {id: t.id}

selfJoinSelectQuery :: SelectQuery (aId :: String, bId ::String)
selfJoinSelectQuery = do
  a <- from testTable
  b <- innerJoin testTable (\b -> b.id :== a.id)
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

    let testDateString = "1832-01-27T14:00:20.012Z"
        testDate = toDateTime $ unsafePartial $ fromJust $ instant $ Milliseconds $ -4352608779988.0
        dfcc = defaultFromConstantConfig

    describe "String" do
      it "toConstant" do
        toConstant "abc" `shouldEqual` StringConstant "abc"
      it "fromConstant" do
        fromConstant dfcc (StringConstant "abc") `shouldEqual` Just "abc"
        fromConstant dfcc (IntConstant 123) `shouldEqual` Nothing :: Maybe String
        fromConstant dfcc (NumberConstant 123.0) `shouldEqual` Nothing :: Maybe String
        fromConstant dfcc (DateTimeConstant testDate) `shouldEqual` Nothing :: Maybe String
        fromConstant dfcc NullConstant `shouldEqual` Nothing :: Maybe String

    describe "Int" do
      it "toConstant" do
        toConstant 123 `shouldEqual` IntConstant 123
      it "fromConstant" do
        fromConstant dfcc (IntConstant 123) `shouldEqual` Just 123
        fromConstant dfcc (StringConstant "abc") `shouldEqual` Nothing :: Maybe Int
        fromConstant dfcc (NumberConstant 123.0) `shouldEqual` Nothing :: Maybe Int
        fromConstant dfcc (DateTimeConstant testDate) `shouldEqual` Nothing :: Maybe Int
        fromConstant dfcc NullConstant `shouldEqual` Nothing :: Maybe Int

    describe "Number" do
      it "toConstant" do
        toConstant 123.456 `shouldEqual` NumberConstant 123.456
      it "fromConstant" do
        fromConstant dfcc (NumberConstant 123.0) `shouldEqual` Just 123.0
        fromConstant dfcc (IntConstant 123) `shouldEqual` Just 123.0
        fromConstant dfcc (StringConstant "abc") `shouldEqual` Nothing :: Maybe Number
        fromConstant dfcc (DateTimeConstant testDate) `shouldEqual` Nothing :: Maybe Number
        fromConstant dfcc NullConstant `shouldEqual` Nothing :: Maybe Number

    describe "Boolean" do
      it "toConstant" do
        toConstant false `shouldEqual` IntConstant 0
        toConstant true `shouldEqual` IntConstant 1
      it "fromConstant" do
        fromConstant dfcc (NumberConstant 123.0) `shouldEqual` Nothing :: Maybe Boolean
        fromConstant dfcc (StringConstant "abc") `shouldEqual` Nothing :: Maybe Boolean
        fromConstant dfcc (IntConstant 0) `shouldEqual` Just false
        fromConstant dfcc (IntConstant 1) `shouldEqual` Just true
        fromConstant dfcc (IntConstant 42) `shouldEqual` Just true
        fromConstant dfcc (DateTimeConstant testDate) `shouldEqual` Nothing :: Maybe Boolean
        fromConstant dfcc NullConstant `shouldEqual` Nothing :: Maybe Boolean

    describe "Maybe" do
      it "toConstant" do
        toConstant (Nothing :: Maybe Int) `shouldEqual` NullConstant
        toConstant (Just 1) `shouldEqual` IntConstant 1
      it "fromConstant" do
        fromConstant dfcc NullConstant `shouldEqual` Just (Nothing :: Maybe Int)
        fromConstant dfcc (IntConstant 123) `shouldEqual` Just (Just 123)
        fromConstant dfcc (StringConstant "abc") `shouldEqual` Nothing :: Maybe Int

    describe "DateTime" do
      it "toConstant" do
        toConstant testDate `shouldEqual` DateTimeConstant testDate
      it "fromConstant - default config" do
        fromConstant dfcc (DateTimeConstant testDate) `shouldEqual` Just testDate
        fromConstant dfcc (StringConstant testDateString) `shouldEqual` Nothing :: Maybe DateTime
        fromConstant dfcc (NumberConstant 123.0) `shouldEqual` Nothing :: Maybe DateTime
        fromConstant dfcc (IntConstant 123) `shouldEqual` Nothing :: Maybe DateTime
        fromConstant dfcc NullConstant `shouldEqual` Nothing :: Maybe DateTime

      it "fromConstant - with custom unformatDateTime config" do
        let config = { unformatDateTime: \s -> if s == "a" then Just testDate else Nothing }
        fromConstant config (StringConstant "a") `shouldEqual` Just testDate
        fromConstant config (StringConstant "b") `shouldEqual` Nothing :: Maybe DateTime

    describe "OctetArray" do
      let octets = [1, 2, 3]
      it "toConstant" do
        toConstant octets `shouldEqual` OctetArrayConstant octets
      it "fromConstant" do
        fromConstant dfcc (OctetArrayConstant octets) `shouldEqual` Just octets
        fromConstant dfcc (StringConstant "abc") `shouldEqual` Nothing :: Maybe (Array Octet)

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
        "select b.extra, b.id as jId, a.id as tId from test as a join child as b on (b.id = a.id)"
        []

    it "leftJoinSelectQuery" do
      toSql leftJoinSelectQuery `shouldBeSql` ParameterizedSql
        "select b.extra, b.id as jId, a.id as tId from test as a left join child as b on (b.id = a.id)"
        []

    it "crossJoinSelectQuery" do
      toSql crossJoinSelectQuery `shouldBeSql` ParameterizedSql
        "select b.extra, b.id as jId, a.id as tId from test as a cross join child as b"
        []

    it "selfJoinSelectQuery" do
      toSql selfJoinSelectQuery `shouldBeSql` ParameterizedSql
        "select a.id as aId, b.id as bId from test as a join test as b on (b.id = a.id)" []

    it "selectQueryWithOrderBy" do
      toSql selectQueryWithOrderBy `shouldBeSql` ParameterizedSql
        "select a.id from test as a order by a.description asc, a.id desc" []

    it "selectQueryWithGroupBy" do
      toSql selectQueryWithGroupBy `shouldBeSql` ParameterizedSql
        "select a.id, count(*) as n from test as a group by a.description" []

    it "selectQueryWithTwoGroupBys" do
      toSql selectQueryWithTwoGroupBys `shouldBeSql` ParameterizedSql
        "select a.id, count(*) as n from test as a group by a.description, a.id" []

    it "selectQueryWithGroupByHaving" do
      toSql selectQueryWithGroupByHaving `shouldBeSql` ParameterizedSql
        "select a.id, count(*) as n from test as a group by a.description having (count(*) >= ?)" [c 5]

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
      toSql selectQueryWithNoFrom `shouldBeSql` ParameterizedSql
        "select ? as int, ? as string" [c 123, c "abc"]

    it "selectQueryWithFirstTableAsJoin" do
      toSql selectQueryWithFirstTableAsJoin `shouldEqual`
        Left "A join condition cannot be supplied for the first table in the from clause"

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
        idNameProxy = RProxy :: RProxy (id :: Int, name :: String)
        dfcc = defaultFromConstantConfig

    it "fields match" do
      constantsToRecord idNameProxy dfcc (Map.insert "id" (c 123) $ Map.singleton "name" (c "abc"))
        `shouldEqual` Right {id: 123, name: "abc"}

    it "extra field value supplied" do
      constantsToRecord idProxy dfcc (Map.insert "id" (c 123) $ Map.singleton "extra" (c 456))
        `shouldEqual` Left "Value supplied for unknown field: extra = 456"

    it "expected field value missing" do
      constantsToRecord idNameProxy dfcc (Map.singleton "id" (c 123))
        `shouldEqual` Left "No value found for required field: name"

    it "field value with incorrect type" do
      constantsToRecord idProxy dfcc (Map.singleton "id" (c "123"))
        `shouldEqual` Left "Value has incorrect type for field id, unable to convert: \"123\""

test :: Spec Unit
test = do
  describe "QueryDsl" do
    sqlGeneration
    resultGeneration
    sqlType
    Expressions.test
    SQLite3.test
