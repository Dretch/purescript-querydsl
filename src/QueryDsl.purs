module QueryDsl (
  class SqlType,
  sqlTypeSyntax,
  toConstant,
  Constant,
  Table,
  TypedColumn,
  Column,
  ColumnName,
  SelectQuery,
  InsertQuery,
  DeleteQuery,
  Expression,
  UntypedExpression,
  class ToExpression,
  BinaryOperator,
  UnaryOperator,
  class ValuesMatchColumns,
  getColumnValues,
  class ApplyValuesMatchColumns,
  getColumnValues',
  toExpression,
  alwaysTrue,
  makeTable,
  addColumn,
  column,
  selectFrom,
  class SelectExpressions,
  getSelectExpressions,
  class ApplySelectExpressions,
  getSelectExpressions',
  insertInto,
  deleteFrom,
  prefixOperator,
  postfixOperator,
  binaryOperator,
  from,
  createTableSql,
  selectSql,
  insertSql,
  deleteSql,
  expressionSql) where

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.List (List(..), snoc, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), joinWith)
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (show, ($), (<$>), (<>), (>>>))
import Prim.Row (class Cons, class Lacks)
import Record as Record
import Type.Row (class RowToList, Cons, Nil, kind RowList, RLProxy(..), RProxy(..))
import Undefined (undefined)

class SqlType t where
  sqlTypeSyntax :: t -> { typ :: String, nullable :: Boolean }
  toConstant :: t -> Constant

-- TODO: fromConstant!

instance sqlTypeString :: SqlType String where
  sqlTypeSyntax _ = { typ: "text", nullable: false }
  toConstant = StringConstant

instance sqlTypeInt :: SqlType Int where
  sqlTypeSyntax _ = { typ: "integer", nullable: false }
  toConstant = IntConstant

instance sqlTypeNumber :: SqlType Number where
  sqlTypeSyntax _ = { typ: "real", nullable: false }
  toConstant = NumberConstant

instance sqlTypeBoolean :: SqlType Boolean where
  sqlTypeSyntax _ = { typ: "integer", nullable: false }
  toConstant true = IntConstant 1
  toConstant false = IntConstant 0

instance sqlTypeMaybe :: SqlType a => SqlType (Maybe a) where
  sqlTypeSyntax _ = { typ: (sqlTypeSyntax $ undefined :: a).typ, nullable: true }
  toConstant (Just x) = toConstant x
  toConstant Nothing = NullConstant

newtype TableName = TableName String

newtype ColumnName = ColumnName String

data Table (cols :: #Type) = Table TableName (List UnTypedColumn) (Record cols)

data UnTypedColumn = UnTypedColumn TableName ColumnName String -- ???

newtype TypedColumn (name :: Symbol) typ = TypedColumn UnTypedColumn

newtype Column (name :: Symbol) typ = Column (SProxy name)

-- | A query that select data from a table
data SelectQuery (results :: #Type) = SelectQuery TableName (List (Tuple ColumnName UntypedExpression)) (Expression Boolean)

-- | A query that deletes data from a table
data DeleteQuery = DeleteQuery TableName (Expression Boolean)

data InsertQuery = InsertQuery TableName (List (Tuple ColumnName Constant))

data Constant = StringConstant String
              | IntConstant Int
              | NumberConstant Number
              | NullConstant

data UntypedExpression = PrefixOperatorExpr String UntypedExpression
                       | PostfixOperatorExpr String UntypedExpression
                       | BinaryOperatorExpr String UntypedExpression UntypedExpression
                       | ConstantExpr Constant
                       | ColumnExpr UnTypedColumn
                       | AlwaysTrueExpr

newtype Expression result = Expression UntypedExpression

class ToExpression a result | a -> result where
  toExpression :: a -> Expression result

instance toExpressionTypedColumn :: ToExpression (TypedColumn name typ) typ where
  toExpression (TypedColumn c) = Expression (ColumnExpr c)

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

alwaysTrue :: Expression Boolean
alwaysTrue = Expression AlwaysTrueExpr

makeTable :: String -> Table ()
makeTable name = Table (TableName name) Nil {}

addColumn :: forall name typ oldCols newCols.
  SqlType typ =>
  IsSymbol name =>
  Lacks name oldCols =>
  Cons name (TypedColumn name typ) oldCols newCols =>
  Table oldCols ->
  Column name typ ->
  Table newCols
addColumn (Table tName cols rec) (Column cName) =
  let {typ, nullable} = sqlTypeSyntax (undefined :: typ)
      null = if nullable then "" else " not null"
      col = UnTypedColumn tName (ColumnName $ reflectSymbol cName) (typ <> null)
      typedCol = TypedColumn col
  in Table tName (snoc cols col) (Record.insert cName typedCol rec)

column :: forall name typ. Column name typ
column = Column SProxy

from :: forall cols. Table cols -> Record cols
from (Table name cols rec) = rec

class SelectExpressions (exprs :: #Type) (result :: #Type) | exprs -> result where
  getSelectExpressions :: Record exprs -> List (Tuple ColumnName UntypedExpression)

instance selectExpressionsImpl
  :: ( RowToList exprsR exprsRL
     , RowToList resultsR resultsRL
     , ApplySelectExpressions exprsRL exprsR resultsRL ) => SelectExpressions exprsR resultsR
  where
    getSelectExpressions exprs = getSelectExpressions'
      (RLProxy :: RLProxy exprsRL) exprs (RLProxy :: RLProxy resultsRL)

class ApplySelectExpressions (exprsRL :: RowList) (exprsR :: #Type) (resultsRL :: RowList) where
  getSelectExpressions' :: RLProxy exprsRL -> Record exprsR -> RLProxy resultsRL -> List (Tuple ColumnName UntypedExpression)

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
          (tailExprs :: Record exprsRTail)
          (RLProxy :: RLProxy resultsRLTail)

selectFrom :: forall cols exprs results. SelectExpressions exprs results => Table cols -> Record exprs -> Expression Boolean -> SelectQuery results
selectFrom (Table tName _ _) exprs filter =
  SelectQuery tName (getSelectExpressions exprs) filter

deleteFrom :: forall cols. Table cols -> Expression Boolean -> DeleteQuery
deleteFrom (Table tName  _ _) filter' = DeleteQuery tName filter'

insertInto :: forall cols vals. ValuesMatchColumns cols vals => Table cols -> Record vals -> InsertQuery
insertInto (Table tName _ _) vals =
  InsertQuery tName columnValues
  where
    columnValues = List.reverse $ getColumnValues vals

untypeExpression :: forall a. Expression a -> UntypedExpression
untypeExpression (Expression ut) = ut

prefixOperator :: forall a input result. String -> UnaryOperator a input result
prefixOperator op a = Expression $ PrefixOperatorExpr op (untypeExpression $ toExpression a)

postfixOperator :: forall a input result. String -> UnaryOperator a input result
postfixOperator op a = Expression $ PostfixOperatorExpr op (untypeExpression $ toExpression a)

binaryOperator :: forall a b input result. String -> BinaryOperator a b input result
binaryOperator op a b = Expression $ BinaryOperatorExpr op (untypeExpression $ toExpression a) (untypeExpression $ toExpression b)

createTableSql :: forall cols. Table cols -> String
createTableSql (Table tName cols rec) =
  "create table " <> tableNameSql tName <> " (" <> columnsSql <> ")"
  where
    columnsSql = joinCsv $ columnSql <$> cols
    columnSql (UnTypedColumn _ (ColumnName cName) createSql) =
      cName <> " " <> createSql

selectSql :: forall cols. SelectQuery cols -> String
selectSql (SelectQuery tName cols filter') =
  "select " <> selectClause <> " from " <> tableNameSql tName <> whereClause
  where
    selectClause = joinCsv $ selectCol <$> cols

    selectCol (Tuple _ (ColumnExpr (UnTypedColumn tName' cName _))) = tableColumnNameSql tName' cName
    selectCol (Tuple cName expr) = untypedExpressionSql expr <> " as " <> columnNameSql cName

    whereClause = case filter' of
      Expression AlwaysTrueExpr -> ""
      expr -> " where " <> expressionSql expr

deleteSql :: DeleteQuery -> String
deleteSql (DeleteQuery tName filter') =
  "delete from " <> tableNameSql tName <> " where " <> expressionSql filter'

class ValuesMatchColumns (cols :: #Type) (vals :: #Type) | cols -> vals, vals -> cols where
  getColumnValues :: Record vals -> List (Tuple ColumnName Constant)

instance valuesMatchColumnsImpl
 :: ( RowToList colsR colsRL
    , RowToList valsR valsRL
    , ApplyValuesMatchColumns colsRL valsRL valsR ) => ValuesMatchColumns colsR valsR
  where
    getColumnValues rec = getColumnValues'
     (RLProxy :: RLProxy colsRL)
     (RLProxy :: RLProxy valsRL)
     (RProxy :: RProxy valsR)
     rec

class ApplyValuesMatchColumns (colsRL :: RowList) (valsRL :: RowList) (valsR :: #Type)
  where
    getColumnValues' :: RLProxy colsRL -> RLProxy valsRL -> RProxy valsR -> Record valsR -> List (Tuple ColumnName Constant)

instance applyValuesMatchColumnsNil
  :: ApplyValuesMatchColumns Nil Nil valsR
    where
      getColumnValues' _ _ _ _ = Nil

instance applyValuesMatchColumnsCons
  :: ( ApplyValuesMatchColumns colsRLTail valsRLTail valsRTail
     , IsSymbol name
     , SqlType typ
     , Cons name typ valsRTail valsR
     , Lacks name valsRTail ) =>
     ApplyValuesMatchColumns (Cons name (TypedColumn name typ) colsRLTail) (Cons name typ valsRLTail) valsR
    where
      getColumnValues' _ _ _ rec =
        Tuple cName cValue : tail
        where
          nameProxy = SProxy :: SProxy name
          cName = ColumnName $ reflectSymbol nameProxy
          cValue = toConstant $ Record.get nameProxy rec
          tailRec = Record.delete nameProxy rec
          tail = getColumnValues'
            (RLProxy :: RLProxy colsRLTail)
            (RLProxy :: RLProxy valsRLTail)
            (RProxy :: RProxy valsRTail)
            tailRec

insertSql :: InsertQuery -> String
insertSql (InsertQuery tName columnValues) =
  "insert into " <> tableNameSql tName <> " (" <> colsSql <> ") values (" <> valuesSql <> ")"
  where
    colsSql = joinCsv $ (fst >>> columnNameSql) <$> columnValues
    valuesSql = joinCsv $ (snd >>> constantSql) <$> columnValues

tableNameSql :: TableName -> String
tableNameSql (TableName name) = name -- TODO: quote if neccessary

columnNameSql :: ColumnName -> String
columnNameSql (ColumnName name) = name -- TODO: quote if neccessary

tableColumnNameSql :: TableName -> ColumnName -> String
tableColumnNameSql tName cName = tableNameSql tName <> "." <> columnNameSql cName

expressionSql :: forall result. Expression result -> String
expressionSql (Expression e) = untypedExpressionSql e

untypedExpressionSql :: UntypedExpression -> String
untypedExpressionSql (PrefixOperatorExpr op a) = "(" <> op <> " " <> untypedExpressionSql a <> ")"
untypedExpressionSql (PostfixOperatorExpr op a) = "(" <> untypedExpressionSql a <> " " <> op <> ")"
untypedExpressionSql (BinaryOperatorExpr op a b) = "(" <> untypedExpressionSql a <> " " <> op <> " " <> untypedExpressionSql b <> ")"
untypedExpressionSql (ConstantExpr c) = constantSql c
untypedExpressionSql (ColumnExpr (UnTypedColumn tName cName _)) = tableColumnNameSql tName cName
untypedExpressionSql AlwaysTrueExpr = "(1 = 1)"

-- TODO: don't turn constants into SQL strings! instead send them to the SQL server as parameters
constantSql :: Constant -> String
constantSql (StringConstant s) = "'" <> String.replaceAll (Pattern "'") (Replacement "''") s <> "'"
constantSql (IntConstant i) = show i
constantSql (NumberConstant f) = show f
constantSql NullConstant = "null"

joinCsv :: forall f. Foldable f => f String -> String
joinCsv = Array.fromFoldable >>> joinWith ", "
