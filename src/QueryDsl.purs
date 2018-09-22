module QueryDsl (
  class SqlType,
  sqlTypeSyntax,
  toConstant,
  Constant,
  Table,
  TypedColumn,
  Column,
  SelectQuery,
  DeleteQuery,
  Expression,
  class ToExpression,
  BinaryOperator,
  UnaryOperator,
  toExpression,
  makeTable,
  addColumn,
  column,
  select,
  selectPlus,
  (++),
  deleteFrom,
  prefixOperator,
  postfixOperator,
  binaryOperator,
  from,
  filter,
  createTableSql,
  selectSql,
  deleteSql,
  expressionSql) where

import Data.Array as Array
import Data.List (List(..), nub, singleton, snoc)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), Replacement(..), joinWith)
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prelude (show, ($), (<$>), (<>), (>>>))
import Prim.Row (class Cons, class Lacks)
import Record as Record
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

-- | A query that returns a table with the given columns
data SelectQuery (r :: #Type) = SelectQuery (List UnTypedColumn) (Maybe (Expression Boolean))

data DeleteQuery = DeleteQuery TableName (Expression Boolean)

data Constant = StringConstant String
              | IntConstant Int
              | NumberConstant Number
              | NullConstant

data UntypedExpression = PrefixOperatorExpr String UntypedExpression
                       | PostfixOperatorExpr String UntypedExpression
                       | BinaryOperatorExpr String UntypedExpression UntypedExpression
                       | ConstantExpr Constant
                       | ColumnExpr UnTypedColumn

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

select :: forall name typ result.
  Cons name typ () result => TypedColumn name typ -> SelectQuery result
select (TypedColumn uc) = SelectQuery (singleton uc) Nothing

selectPlus :: forall name typ start result.
  Cons name typ start result =>
  SelectQuery start ->
  TypedColumn name typ ->
  SelectQuery result
selectPlus (SelectQuery start filter') (TypedColumn c) = SelectQuery (snoc start c) filter'

infixl 5 selectPlus as ++

deleteFrom :: forall cols. Table cols -> Expression Boolean -> DeleteQuery
deleteFrom (Table tName  _ _) filter' = DeleteQuery tName filter'

filter :: forall result. SelectQuery result -> Expression Boolean -> SelectQuery result
filter (SelectQuery selector Nothing) exp = SelectQuery selector (Just exp)
filter (SelectQuery selector (Just exp)) exp' = SelectQuery selector (Just $ binaryOperator "and" exp' exp)

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
    columnsSql = joinWith ", " $ Array.fromFoldable $ columnSql <$> cols
    columnSql (UnTypedColumn _ (ColumnName cName) createSql) =
      cName <> " " <> createSql

selectSql :: forall cols. SelectQuery cols -> String
selectSql (SelectQuery cols filter') =
  "select " <> selectClause <> " from " <> fromClause <> whereClause
  where
    selectClause = joinWith ", " $ Array.fromFoldable $ selectCol <$> cols
    selectCol (UnTypedColumn (TableName tName) (ColumnName cName) _) =
      tName <> "." <> cName

    fromClause = joinWith ", " $ Array.fromFoldable tables
    tables = nub $ (\(UnTypedColumn tName _ _) -> tableNameSql tName) <$> cols

    whereClause = maybe "" (expressionSql >>> (" where " <> _)) filter'

deleteSql :: DeleteQuery -> String
deleteSql (DeleteQuery tName filter') =
  "delete from " <> tableNameSql tName <> " where " <> expressionSql filter'

tableNameSql :: TableName -> String
tableNameSql (TableName name) = name -- TODO: quote if neccessary (for column names too)

expressionSql :: forall result. Expression result -> String
expressionSql (Expression e) = exprSql e
  where
    exprSql :: UntypedExpression -> String
    exprSql (PrefixOperatorExpr op a) = "(" <> op <> " " <> exprSql a <> ")"
    exprSql (PostfixOperatorExpr op a) = "(" <> exprSql a <> " " <> op <> ")"
    exprSql (BinaryOperatorExpr op a b) = "(" <> exprSql a <> " " <> op <> " " <> exprSql b <> ")"
    exprSql (ConstantExpr c) = constantSql c
    exprSql (ColumnExpr (UnTypedColumn (TableName t) (ColumnName c) _)) = t <> "." <> c

    constantSql :: Constant -> String
    constantSql (StringConstant s) = "'" <> String.replaceAll (Pattern "'") (Replacement "''") s <> "'"
    constantSql (IntConstant i) = show i
    constantSql (NumberConstant f) = show f
    constantSql NullConstant = "null"
