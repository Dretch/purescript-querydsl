-- | A type-safe and composable SQL query builder.
module QueryDsl (
  Constant(..),
  class SqlType,
  toConstant,
  fromConstant,
  FromConstantConfig,
  defaultFromConstantConfig,
  Table,
  TableName,
  Column,
  ColumnName,
  SelectQuery,
  SelectTableBuilder,
  SelectEndpoint,
  UpdateQuery,
  InsertQuery,
  DeleteQuery,
  Expression,
  UntypedExpression,
  class ToExpression,
  BinaryOperator,
  UnaryOperator,
  ParameterizedSql(..),
  ErrorMessage,
  toExpression,
  alwaysTrue,
  class TableColumns,
  getTableColumns,
  class ApplyTableColumns,
  getTableColumns',
  makeTable,
  from,
  innerJoin,
  leftJoin,
  crossJoin,
  select,
  where_,
  limit,
  offset,
  OrderingExpression,
  asc,
  desc,
  orderBy,
  groupBy,
  having,
  update,
  insertInto,
  deleteFrom,
  class InsertExpressions,
  getInsertExpressions,
  class ApplyInsertExpressions,
  getInsertExpressions',
  class SelectExpressions,
  getSelectExpressions,
  class ApplySelectExpressions,
  getSelectExpressions',
  class UpdateExpressions,
  getUpdateExpressions,
  class ApplyUpdateExpressions,
  getUpdateExpressions',
  prefixOperator,
  postfixOperator,
  binaryOperator,
  unaryAggregateFunction,
  nullaryFunction,
  columns,
  class Query,
  toSql,
  expressionSql,
  class ConstantsToRecord,
  constantsToRecord,
  class ApplyConstantsToRecord,
  constantsToRecord') where

import Prelude

import Control.Monad.State (State, runState, get, put)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.CodeUnits (charAt, singleton)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Node.Buffer (Octet)
import Prim.Row (class Cons, class Lacks)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as RB
import Type.Data.Boolean (kind Boolean, False, True)
import Type.Row (RProxy(..))
import Type.RowList (class RowToList, Cons, Nil, RLProxy(..), kind RowList)

-- | Values that can be stored in database columns
data Constant = StringConstant String
              | IntConstant Int
              | NumberConstant Number
              | DateTimeConstant DateTime
              | OctetArrayConstant (Array Octet)
              | NullConstant

derive instance eqConstant :: Eq Constant

instance showConstant :: Show Constant where
  show (StringConstant s) = show s
  show (IntConstant i) = show i
  show (NumberConstant f) = show f
  show (DateTimeConstant dt) = show dt
  show (OctetArrayConstant oa) = show oa
  show NullConstant = "null"

-- | Passed to `fromConstant` to configure how values are converted from constants.
type FromConstantConfig = { unformatDateTime :: String -> Maybe DateTime }

-- | The default configuration does nothing.
defaultFromConstantConfig :: FromConstantConfig
defaultFromConstantConfig = { unformatDateTime : const Nothing }

-- | Types that can be converted to/from Constant values
class SqlType t where
  toConstant :: t -> Constant
  fromConstant :: FromConstantConfig -> Constant -> Maybe t

instance sqlTypeString :: SqlType String where
  toConstant = StringConstant
  fromConstant _ (StringConstant s) = Just s
  fromConstant _ _ = Nothing

instance sqlTypeInt :: SqlType Int where
  toConstant = IntConstant
  fromConstant _ (IntConstant n) = Just n
  fromConstant _ _ = Nothing

instance sqlTypeNumber :: SqlType Number where
  toConstant = NumberConstant
  fromConstant _ (NumberConstant n) = Just n
  fromConstant _ (IntConstant n) = Just $ Int.toNumber n
  fromConstant _ _ = Nothing

instance sqlTypeBoolean :: SqlType Boolean where
  toConstant true = IntConstant 1
  toConstant false = IntConstant 0
  fromConstant _ (IntConstant 0) = Just false
  fromConstant _ (IntConstant _) = Just true
  fromConstant _ _ = Nothing

instance sqlTypeDateTime :: SqlType DateTime where
  toConstant = DateTimeConstant
  fromConstant _ (DateTimeConstant dt) = Just dt
  fromConstant { unformatDateTime } (StringConstant s) = unformatDateTime s
  fromConstant _ _ = Nothing

instance sqlTypeMaybe :: SqlType a => SqlType (Maybe a) where
  toConstant (Just x) = toConstant x
  toConstant Nothing = NullConstant
  fromConstant _ NullConstant = Just Nothing
  fromConstant config c = Just <$> fromConstant config c

instance sqlTypeOctetArray :: SqlType (Array Int) where
  toConstant = OctetArrayConstant
  fromConstant _ (OctetArrayConstant oa) = Just oa
  fromConstant _ _ = Nothing

newtype TableName = TableName String

derive newtype instance eqTableName :: Eq TableName

newtype ColumnName = ColumnName String

derive newtype instance eqColumnName :: Eq ColumnName

-- | A table definition, with a row-type parameter that captures the types of the columns in the table.
data Table (cols :: #Type) = Table TableName (TableName -> { | cols })

data UnTypedColumn = UnTypedColumn TableName ColumnName

-- | A column within a table, with the column type and optionality represented in the type of the Column
newtype Column typ (required :: Boolean) = Column UnTypedColumn

-- | A query that selects data, with the type of the columns in the result represented in the type of the SelectQuery
type SelectQuery results = SelectTableBuilder (SelectEndpoint results)

-- | A Monad for constructing a SQL from/join clause
newtype SelectTableBuilder a = SelectTableBuilder (State (List SelectTable) a)

derive newtype instance functorSelectTableBuilder :: Functor SelectTableBuilder
derive newtype instance applySelectTableBuilder :: Apply SelectTableBuilder
derive newtype instance applicativeSelectTableBuilder :: Applicative SelectTableBuilder
derive newtype instance bindSelectTableBuilder :: Bind SelectTableBuilder

type SelectTable = {
  table :: TableName,
  alias :: TableName,
  join :: Join
}

data Join = Initial
          | InnerJoin (Expression Boolean)
          | LeftOuterJoin (Expression Boolean)
          | CrossJoin

-- | The select columns and the where-clause/order-by/limit/etc part of a select query
data SelectEndpoint (results :: #Type) = SelectEndpoint {
  columns :: List (Tuple ColumnName UntypedExpression),
  where_ :: Expression Boolean,
  orderBy :: Array OrderingExpression,
  groupBy :: Array UntypedExpression,
  having :: Expression Boolean,
  limit :: Maybe Int,
  offset :: Maybe Int
}

-- | An expression that determines how the results are ordered.
data OrderingExpression = Asc UntypedExpression
                        | Desc UntypedExpression

data UpdateQuery = UpdateQuery TableName (List (Tuple ColumnName UntypedExpression)) (Expression Boolean)

-- | A query that deletes data from a table
data DeleteQuery = DeleteQuery TableName (Expression Boolean)

data InsertQuery = InsertQuery TableName (List (Tuple ColumnName Constant))

data UntypedExpression = PrefixOperatorExpr String UntypedExpression
                       | PostfixOperatorExpr String UntypedExpression
                       | BinaryOperatorExpr String UntypedExpression UntypedExpression
                       | UnaryAggregateFunctionExpr String Boolean UntypedExpression
                       | NullaryFunctionExpr String
                       | ConstantExpr Constant
                       | ColumnExpr UnTypedColumn
                       | AlwaysTrueExpr

newtype Expression result = Expression UntypedExpression

class ToExpression a result | a -> result where
  toExpression :: a -> Expression result

instance toExpressionColumn :: ToExpression (Column typ required) typ where
  toExpression (Column c) = Expression (ColumnExpr c)

else instance toExpressionExpression :: ToExpression (Expression result) result where
  toExpression e = e

else instance toExpressionConstant :: SqlType a => ToExpression a a where
  toExpression s = Expression (ConstantExpr $ toConstant s)

type UnaryOperator a input result =
  ToExpression a input =>
  a -> Expression result

type BinaryOperator a b input result =
  ToExpression a input =>
  ToExpression b input =>
  a -> b -> Expression result

data ParameterizedSql = ParameterizedSql String (Array Constant)

type ErrorMessage = String

derive instance eqParameterizedSql :: Eq ParameterizedSql

instance showParameterizedSql :: Show ParameterizedSql where
  show (ParameterizedSql sql parameters) = show sql <> " with " <> show parameters

-- | An expression that always evaluates to `true`. This is useful when a filtering
-- | expression argument is required but you don't want to actually filter the result.
alwaysTrue :: Expression Boolean
alwaysTrue = Expression AlwaysTrueExpr

-- | An instance of this type class is automatically derived by the compiler for
-- | rows where each field in the row matches `name: Column typ required`, representing
-- | a database table column called `name`, having a database type compatible with
-- | the Purescript type `typ`, and being required or optional on insert according to `required`
class TableColumns (cols :: #Type) where
  getTableColumns :: RProxy cols -> TableName -> { | cols }

instance tableColumnsImpl
  :: ( RowToList colsR colsRL
     , ApplyTableColumns colsRL colsR ) => TableColumns colsR
  where
    getTableColumns rProxy tName =
      let builder = getTableColumns' (RLProxy :: RLProxy colsRL) rProxy tName
      in RB.build builder {}

class ApplyTableColumns (colsRL :: RowList) (colsR :: #Type) | colsRL -> colsR where
  getTableColumns' :: RLProxy colsRL -> RProxy colsR -> TableName -> Builder {} { | colsR }

instance applyTableColumnsNil :: ApplyTableColumns Nil () where
  getTableColumns' _ _ _ = identity

instance applyTableColumnsCons
  :: ( ApplyTableColumns colsRLTail colsRTail
     , SqlType typ
     , IsSymbol name
     , Cons name (Column typ required) colsRTail colsR
     , Lacks name colsRTail ) =>
     ApplyTableColumns (Cons name (Column typ required) colsRLTail) colsR
  where
    getTableColumns' _ _ tName =
      RB.insert nameProxy col <<< tail
      where
        nameProxy = SProxy :: SProxy name
        cName = ColumnName $ reflectSymbol nameProxy
        col = Column $ UnTypedColumn tName cName
        tail = getTableColumns'
          (RLProxy :: RLProxy colsRLTail)
          (RProxy :: RProxy colsRTail)
          tName

-- | Makes a table with a given name, taking the column information from the declared type.
makeTable :: forall cols. TableColumns cols => String -> Table cols
makeTable name =
  Table (TableName name) (getTableColumns (RProxy :: RProxy cols))

-- | Gets the columns associated with a table.
columns :: forall cols. Table cols -> { | cols }
columns (Table name rec) = rec name

-- | Create an OrderingExpression that says to order by the given expression in ascending order
asc :: forall a b. ToExpression a b => a -> OrderingExpression
asc e = Asc $ untypeExpression $ toExpression e

-- | Create an OrderingExpression that says to order by the given expression in descending order
desc :: forall a b. ToExpression a b => a -> OrderingExpression
desc e = Desc $ untypeExpression $ toExpression e

-- | Starts a SelectTableBuilder by specifying the initial table.
from :: forall cols. Table cols -> SelectTableBuilder { | cols }
from table = addTable table (const Initial)

-- | Extends a SelectTableBuilder by specifying an (inner) join table.
innerJoin :: forall cols. Table cols -> ({ | cols } -> Expression Boolean) -> SelectTableBuilder { | cols }
innerJoin table expr = addTable table (expr >>> InnerJoin)

-- | Extends a SelectTableBuilder by specifying a (left outer) join table.
leftJoin :: forall cols. Table cols -> ({ | cols } -> Expression Boolean) -> SelectTableBuilder { | cols }
leftJoin table expr = addTable table (expr >>> LeftOuterJoin)

-- | Extends a SelectTableBuilder by specifying a (cross) join table.
crossJoin :: forall cols. Table cols -> SelectTableBuilder { | cols }
crossJoin table = addTable table (const CrossJoin)

addTable :: forall cols. Table cols -> ({ | cols } -> Join) -> SelectTableBuilder { | cols }
addTable (Table table cols) getJoin = SelectTableBuilder do
  tables <- get
  let alias = makeAlias (List.length tables)
      cols' = cols alias
  put $ { table, alias, join: getJoin cols' } : tables
  pure cols'

class SelectExpressions (exprs :: #Type) (result :: #Type) | exprs -> result where
  getSelectExpressions :: { | exprs } -> List (Tuple ColumnName UntypedExpression)

instance selectExpressionsImpl
  :: ( RowToList exprsR exprsRL
     , RowToList resultsR resultsRL
     , ApplySelectExpressions exprsRL exprsR resultsRL ) => SelectExpressions exprsR resultsR
  where
    getSelectExpressions exprs = getSelectExpressions'
      (RLProxy :: RLProxy exprsRL) exprs (RLProxy :: RLProxy resultsRL)

class ApplySelectExpressions (exprsRL :: RowList) (exprsR :: #Type) (resultsRL :: RowList) where
  getSelectExpressions' :: RLProxy exprsRL -> { | exprsR } -> RLProxy resultsRL -> List (Tuple ColumnName UntypedExpression)

instance applySelectExpressionsNil
  :: ApplySelectExpressions Nil exprsR Nil
  where
    getSelectExpressions' _ _ _ = Nil

instance applySelectExpressionsCons
  :: ( ApplySelectExpressions exprsRLTail exprsRTail resultsRLTail
     , IsSymbol name
     , ToExpression toExpr typ
     , Cons name toExpr exprsRTail exprsR
     , Lacks name exprsRTail ) =>
    ApplySelectExpressions (Cons name toExpr exprsRLTail) exprsR (Cons name typ resultsRLTail)
  where
    getSelectExpressions' _ exprs _ =
      Tuple cName uExpr : tail
      where
        nameProxy = SProxy :: SProxy name
        cName = ColumnName $ reflectSymbol nameProxy
        uExpr = untypeExpression $ toExpression $ Record.get nameProxy exprs
        tailExprs = Record.delete nameProxy exprs
        tail = getSelectExpressions'
          (RLProxy :: RLProxy exprsRLTail)
          (tailExprs :: { | exprsRTail })
          (RLProxy :: RLProxy resultsRLTail)

-- | Creates a new SelectEndpoint with the given selected columns
select :: forall exprs results. SelectExpressions exprs results => { | exprs } -> SelectEndpoint results
select exprs = SelectEndpoint {
  columns: getSelectExpressions exprs,
  where_: alwaysTrue,
  orderBy: [],
  groupBy: [],
  having: alwaysTrue,
  limit: Nothing,
  offset: Nothing
}

-- | Sets the where clause to use on the SelectEndpoint
where_ :: forall results. SelectEndpoint results -> Expression Boolean -> SelectEndpoint results
where_ (SelectEndpoint se) filter =
  SelectEndpoint $ se { where_ = filter }

-- | Sets the ordering to use on the SelectEndpoint
orderBy :: forall results. SelectEndpoint results -> Array OrderingExpression -> SelectEndpoint results
orderBy (SelectEndpoint se) orderBy' =
  SelectEndpoint $ se { orderBy = orderBy' }

-- | Adds a column to the group by clause - note this function is cumulative:
-- | call it multiple times to group by more than one expression.
groupBy :: forall results a b. ToExpression a b => SelectEndpoint results -> a -> SelectEndpoint results
groupBy (SelectEndpoint se) e =
  SelectEndpoint $ se { groupBy = se.groupBy <> [untypeExpression $ toExpression e] }

-- | Sets the having expression to use on the SelectEndpoint.
having :: forall results. SelectEndpoint results -> Expression Boolean -> SelectEndpoint results
having (SelectEndpoint se) filter =
  SelectEndpoint $ se { having = filter }

-- | Sets the limit (maximum number of rows in the result) to use on the SelectEndpoint.
limit :: forall results. SelectEndpoint results -> Int -> SelectEndpoint results
limit (SelectEndpoint se) n =
  SelectEndpoint $ se { limit = Just n }

-- | Sets the offset (how many rows to skip from the result) to use on the SelectEndpoint.
offset :: forall results. SelectEndpoint results -> Int -> SelectEndpoint results
offset (SelectEndpoint se) n =
  SelectEndpoint $ se { offset = Just n }

-- | Instances of this type class are automatically derived by the compiler for
-- | row types pairs where each field in `exprs` matches a column in `cols`
class InsertExpressions (cols :: #Type) (exprs :: #Type) | cols -> exprs, exprs -> cols where
  getInsertExpressions :: { | exprs } -> List (Tuple ColumnName Constant)

instance insertExpressionsImpl
 :: ( RowToList colsR colsRL
    , RowToList exprsR exprsRL
    , ApplyInsertExpressions colsRL exprsRL exprsR ) => InsertExpressions colsR exprsR
  where
    getInsertExpressions rec = getInsertExpressions'
     (RLProxy :: RLProxy colsRL)
     (RLProxy :: RLProxy exprsRL)
     (RProxy :: RProxy exprsR)
     rec

class ApplyInsertExpressions (colsRL :: RowList) (exprsRL :: RowList) (exprsR :: #Type)
  where
    getInsertExpressions' :: RLProxy colsRL -> RLProxy exprsRL -> RProxy exprsR -> { | exprsR } -> List (Tuple ColumnName Constant)

instance applyInsertExpressionsNil
  :: ApplyInsertExpressions Nil Nil exprsR
    where
      getInsertExpressions' _ _ _ _ = Nil

else instance applyInsertExpressionsOptionalValueGiven
  :: ( ApplyInsertExpressions colsRLTail exprsRLTail exprsRTail
     , IsSymbol name
     , SqlType typ
     , Cons name typ exprsRTail exprsR
     , Lacks name exprsRTail ) =>
     ApplyInsertExpressions (Cons name (Column typ False) colsRLTail) (Cons name typ exprsRLTail) exprsR
    where
      getInsertExpressions' _ _ _ rec =
        Tuple cName cValue : tail
        where
          nameProxy = SProxy :: SProxy name
          cName = ColumnName $ reflectSymbol nameProxy
          cValue = toConstant $ Record.get nameProxy rec
          tailRec = Record.delete nameProxy rec
          tail = getInsertExpressions'
            (RLProxy :: RLProxy colsRLTail)
            (RLProxy :: RLProxy exprsRLTail)
            (RProxy :: RProxy exprsRTail)
            tailRec

else instance applyInsertExpressionsOptionalValueNotGiven
  :: ApplyInsertExpressions colsRLTail exprsRL exprsR =>
     ApplyInsertExpressions (Cons name (Column typ False) colsRLTail) exprsRL exprsR
    where
      getInsertExpressions' _ exprsRLProxy exprsRProxy exprsR =
        getInsertExpressions' (RLProxy :: RLProxy colsRLTail) exprsRLProxy exprsRProxy exprsR

else instance applyInsertExpressionsRequiredValue
  :: ( ApplyInsertExpressions colsRLTail exprsRLTail exprsRTail
     , IsSymbol name
     , SqlType typ
     , Cons name typ exprsRTail exprsR
     , Lacks name exprsRTail ) =>
     ApplyInsertExpressions (Cons name (Column typ True) colsRLTail) (Cons name typ exprsRLTail) exprsR
    where
      getInsertExpressions' _ _ _ rec =
        Tuple cName cValue : tail
        where
          nameProxy = SProxy :: SProxy name
          cName = ColumnName $ reflectSymbol nameProxy
          cValue = toConstant $ Record.get nameProxy rec
          tailRec = Record.delete nameProxy rec
          tail = getInsertExpressions'
            (RLProxy :: RLProxy colsRLTail)
            (RLProxy :: RLProxy exprsRLTail)
            (RProxy :: RProxy exprsRTail)
            tailRec

insertInto :: forall cols exprs. InsertExpressions cols exprs => Table cols -> { | exprs } -> InsertQuery
insertInto (Table tName _) exprs =
  InsertQuery tName $ List.reverse $ getInsertExpressions exprs

-- | An instance of this type class is automatically derived by the compiler when
-- | each item in `exprs` matches a column of the same name and type in `cols`.
class UpdateExpressions (cols :: #Type) (exprs :: #Type) where
  getUpdateExpressions :: { | cols } -> { | exprs } -> List (Tuple ColumnName UntypedExpression)

instance updateExpressionsImpl
  :: ( RowToList colsR colsRL
     , RowToList exprsR exprsRL
     , ApplyUpdateExpressions colsRL exprsRL exprsR ) => UpdateExpressions colsR exprsR
  where
    getUpdateExpressions cols exprs = getUpdateExpressions'
      (RLProxy :: RLProxy colsRL) (RLProxy :: RLProxy exprsRL) exprs

class ApplyUpdateExpressions (colsRL :: RowList) (exprsRL :: RowList) (exprsR :: #Type) where
  getUpdateExpressions' :: RLProxy colsRL -> RLProxy exprsRL -> { | exprsR } -> List (Tuple ColumnName UntypedExpression)

instance applyUpdateExpressionsNil
  :: ApplyUpdateExpressions colsRL Nil exprsR
  where
    getUpdateExpressions' _ _ _ = Nil

else instance applyUpdateExpressionsCons
  :: ( ApplyUpdateExpressions colsRLTail exprsRLTail exprsRTail
     , IsSymbol name
     , ToExpression toExpr typ
     , Cons name toExpr exprsRTail exprsR
     , Lacks name exprsRTail ) =>
    ApplyUpdateExpressions (Cons name (Column typ required) colsRLTail) (Cons name toExpr exprsRLTail) exprsR
  where
    getUpdateExpressions' _ _ exprs =
      Tuple cName uExpr : tail
      where
        nameProxy = SProxy :: SProxy name
        cName = ColumnName $ reflectSymbol nameProxy
        uExpr = untypeExpression $ toExpression $ Record.get nameProxy exprs
        tailExprs = Record.delete nameProxy exprs
        tail = getUpdateExpressions'
          (RLProxy :: RLProxy colsRLTail)
          (RLProxy :: RLProxy exprsRLTail)
          (tailExprs :: { | exprsRTail })

update :: forall cols exprs. UpdateExpressions cols exprs => Table cols -> { | exprs } -> Expression Boolean -> UpdateQuery
update (Table tName cols) exprs filter =
  UpdateQuery tName (getUpdateExpressions (cols tName) exprs) filter

deleteFrom :: forall cols. Table cols -> Expression Boolean -> DeleteQuery
deleteFrom (Table tName _) filter' = DeleteQuery tName filter'

untypeExpression :: forall a. Expression a -> UntypedExpression
untypeExpression (Expression ut) = ut

prefixOperator :: forall a input result. String -> UnaryOperator a input result
prefixOperator op a = Expression $ PrefixOperatorExpr op (untypeExpression $ toExpression a)

postfixOperator :: forall a input result. String -> UnaryOperator a input result
postfixOperator op a = Expression $ PostfixOperatorExpr op (untypeExpression $ toExpression a)

binaryOperator :: forall a b input result. String -> BinaryOperator a b input result
binaryOperator op a b = Expression $ BinaryOperatorExpr op (untypeExpression $ toExpression a) (untypeExpression $ toExpression b)

unaryAggregateFunction :: forall a input result. String -> Boolean -> UnaryOperator a input result
unaryAggregateFunction op distinct a = Expression $ UnaryAggregateFunctionExpr op distinct (untypeExpression $ toExpression a)

nullaryFunction :: forall result. String -> Expression result
nullaryFunction op = Expression $ NullaryFunctionExpr op

class Query t where
  toSql :: t -> Either ErrorMessage ParameterizedSql

type SqlWriter = Writer (Array Constant) String

instance querySelectQuery :: Query (SelectTableBuilder (SelectEndpoint results)) where
  toSql (SelectTableBuilder builder) = do
      writer <- uncurry toWriter $ runState builder Nil
      Right $ uncurry ParameterizedSql $ runWriter writer
    where
      toWriter :: SelectEndpoint results -> List SelectTable -> Either ErrorMessage SqlWriter
      toWriter endpoint tables =
        toWriter' endpoint <$> checkTables (List.reverse tables)

      checkTables :: List SelectTable -> Either ErrorMessage (List SelectTable)
      checkTables ts = case ts of
        Nil -> Right ts
        { join: Initial } : _ -> Right ts
        { join: _ } : _ -> Left  "A join condition cannot be supplied for the first table in the from clause"

      toWriter' :: SelectEndpoint results -> List SelectTable -> SqlWriter
      toWriter' (SelectEndpoint endpoint) tables = do
        sc <- selectClause
        fc <- fromClause
        wc <- whereClauseSql endpoint.where_
        gb <- groupByClause
        hv <- havingClause
        ob <- orderByClause
        lc <- limitClause
        pure $ "select " <> sc <> fc <> wc <> gb <> hv <> ob <> lc
        where
          selectClause = joinCsv <$> traverse selectCol endpoint.columns

          selectCol (Tuple cName (ColumnExpr (UnTypedColumn tName' cName'))) =
            let start = tableColumnNameSql tName' cName'
            in pure $ if cName == cName'
                        then start
                        else start <> " as " <> columnNameSql cName
          selectCol (Tuple cName expr) = do
            sql <- untypedExpressionSql expr
            pure $ sql <> " as " <> columnNameSql cName

          fromClause = case tables of
            Nil ->
              pure ""
            rootTable : joinedTables -> do
              jc <- joinSpace <$> traverse joinClause joinedTables
              pure $ " from " <> aliasSql rootTable.table rootTable.alias <> jc

          joinClause {table, alias, join: Initial} =
            pure $ " cross join " <> aliasSql table alias
          joinClause {table, alias, join: CrossJoin} =
            pure $ " cross join " <> aliasSql table alias
          joinClause {table, alias, join: InnerJoin expr'} = do
            e <- expressionSql' expr'
            pure $ " join " <> aliasSql table alias <> " on " <> e
          joinClause {table, alias, join: LeftOuterJoin expr'} = do
            e <- expressionSql' expr'
            pure $ " left join " <> aliasSql table alias <> " on " <> e

          groupByClause = case endpoint.groupBy of
            [] -> pure ""
            exprs -> (" group by " <> _) <$> joinCsv <$> traverse untypedExpressionSql exprs

          havingClause = case endpoint.having of
            Expression AlwaysTrueExpr -> pure ""
            expr -> (" having " <> _) <$> expressionSql' expr

          orderByClause = case endpoint.orderBy of
            [] -> pure ""
            exprs -> (" order by " <> _) <$> joinCsv <$> traverse orderByExpr exprs

          orderByExpr (Asc e) = (_ <> " asc") <$> untypedExpressionSql e
          orderByExpr (Desc e) = (_ <> " desc") <$> untypedExpressionSql e

          limitClause = case endpoint.limit, endpoint.offset of
            Just l, Just o -> do
              tell [IntConstant l, IntConstant o]
              pure " limit ? offset ?"
            Just l, Nothing -> do
              tell [IntConstant l]
              pure " limit ?"
            Nothing, Just o -> do
              tell [IntConstant o]
              pure " limit -1 offset ?"
            Nothing, Nothing ->
              pure ""

          aliasSql name alias =
            tableNameSql name <> " as " <> tableNameSql alias

makeAlias :: Int -> TableName
makeAlias i =
  case charAt i "abcdefghijklmnopqrstuvwxyz" of
    Just c -> TableName $ singleton c
    Nothing -> TableName $ "_" <> show i

instance queryDeleteQuery :: Query DeleteQuery where
  toSql (DeleteQuery tName filter) =
    let Tuple sql parameters = runWriter $ whereClauseSql filter in
    Right $ ParameterizedSql ("delete from " <> tableNameSql tName <> sql) parameters

instance queryInsertQuery :: Query InsertQuery where
  toSql (InsertQuery tName columnValues) =
    Right $ uncurry ParameterizedSql $ runWriter writeSql
    where
      writeSql = do
        vs <- joinCsv <$> traverse (snd >>> constantSql) columnValues
        pure $ "insert into " <> tableNameSql tName <> " (" <> colsSql <> ") values (" <> vs <> ")"

      colsSql = joinCsv $ (fst >>> columnNameSql) <$> columnValues

instance queryUpdateQuery :: Query UpdateQuery where
  toSql (UpdateQuery tName columnValues filter) =
    Right $ uncurry ParameterizedSql $ runWriter writeSql
    where
      writeSql = do
        c <- colsSql
        wc <- whereClauseSql filter
        pure $ "update " <> tableNameSql tName <> " set " <> c <> wc

      colsSql = joinCsv <$> traverse colSql columnValues

      colSql (Tuple cName expr) = do
        e <- untypedExpressionSql expr
        pure $ columnNameSql cName <> " = " <> e

tableNameSql :: TableName -> String
tableNameSql (TableName name) = name -- TODO: quote if neccessary

columnNameSql :: ColumnName -> String
columnNameSql (ColumnName name) = name -- TODO: quote if neccessary

tableColumnNameSql :: TableName -> ColumnName -> String
tableColumnNameSql tName cName =
  tableNameSql tName <> "." <> columnNameSql cName

expressionSql :: forall result. Expression result -> Either ErrorMessage ParameterizedSql
expressionSql e =
  Right $ uncurry ParameterizedSql $ runWriter (expressionSql' e)

expressionSql' :: forall result. Expression result -> SqlWriter
expressionSql' (Expression e) = untypedExpressionSql e

untypedExpressionSql :: UntypedExpression -> SqlWriter
untypedExpressionSql (PrefixOperatorExpr op a) = do
  sql <- untypedExpressionSql a
  pure $ "(" <> op <> " " <> sql <> ")"
untypedExpressionSql (PostfixOperatorExpr op a) = do
  sql <- untypedExpressionSql a
  pure $ "(" <> sql <> " " <> op <> ")"
untypedExpressionSql (BinaryOperatorExpr op a b) = do
  sqlA <- untypedExpressionSql a
  sqlB <- untypedExpressionSql b
  pure $ "(" <> sqlA <> " " <> op <> " " <> sqlB <> ")"
untypedExpressionSql (UnaryAggregateFunctionExpr op distinct a) = do
  sqlA <- untypedExpressionSql a
  let prefix = if distinct then "distinct " else ""
  pure $ op <> "(" <> prefix <> sqlA <> ")"
untypedExpressionSql (NullaryFunctionExpr op) =
  pure op
untypedExpressionSql (ConstantExpr c) =
  constantSql c
untypedExpressionSql (ColumnExpr (UnTypedColumn tName cName)) =
  pure $ tableColumnNameSql tName cName
untypedExpressionSql AlwaysTrueExpr =
  pure "(1 = 1)"

constantSql :: Constant -> SqlWriter
constantSql c = tell [c] $> "?"

whereClauseSql :: Expression Boolean -> SqlWriter
whereClauseSql (Expression AlwaysTrueExpr) = pure ""
whereClauseSql expr = (" where " <> _) <$> expressionSql' expr

class ConstantsToRecord (r :: #Type) where
  constantsToRecord :: RProxy r -> FromConstantConfig -> Map String Constant -> Either ErrorMessage { | r }

instance constantsToRecordImpl
  :: ( RowToList r rl
     , ApplyConstantsToRecord r rl ) => ConstantsToRecord r where
  constantsToRecord r config constants = do
    builder <- constantsToRecord' (RLProxy :: RLProxy rl) config constants
    pure $ RB.build builder {}

class ApplyConstantsToRecord (r :: #Type) (rl :: RowList) | rl -> r where
  constantsToRecord' :: RLProxy rl -> FromConstantConfig -> Map String Constant -> Either ErrorMessage (Builder {} { | r })

instance applyConstantsToRecordNil :: ApplyConstantsToRecord () Nil where
  constantsToRecord' _ _ cols =
    case Map.findMin cols of
      Nothing -> Right identity
      Just {key, value} -> Left $ "Value supplied for unknown field: " <> key <> " = " <> show value

instance applyConstantsToRecordCons
  :: ( ApplyConstantsToRecord rTail rlTail
     , IsSymbol name
     , SqlType typ
     , Cons name typ rTail r
     , Lacks name rTail ) => ApplyConstantsToRecord r (Cons name typ rlTail)
  where
    constantsToRecord' _ config cols =
      case Map.pop name cols of
        Nothing ->
          Left $ "No value found for required field: " <> name
        Just (Tuple c tailMap) ->
          case fromConstant config c of
            Nothing ->
              Left $ "Value has incorrect type for field " <> name <> ", unable to convert: " <> show c
            Just v -> do
              tail <- constantsToRecord' tailRLProxy config tailMap
              Right $ RB.insert nameProxy v <<< tail
      where
        nameProxy = SProxy :: SProxy name
        name = reflectSymbol nameProxy
        tailRLProxy = RLProxy :: RLProxy rlTail

joinSpace :: forall f. Foldable f => f String -> String
joinSpace = Array.fromFoldable >>> joinWith " "

joinCsv :: forall f. Foldable f => f String -> String
joinCsv = Array.fromFoldable >>> joinWith ", "
