-- | Allows running Querydsl queries against SQLite3 databases.
-- |
-- | DateTime values are represented in the database as text columns in ISO8601 format (YYYY-MM-DDTHH:mm:ss.SSSZ)
module QueryDsl.SQLite3 (
  runQuery,
  runSelectManyQuery,
  runSelectOneQuery,
  runSelectMaybeQuery
) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Formatter.DateTime (Formatter, format, parseFormatString, unformat)
import Data.Function.Uncurried as U
import Data.Nullable as Nullable
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Uncurried as EU
import Foreign (Foreign, unsafeToForeign)
import Node.Buffer as Buffer
import Node.Buffer (Buffer)
import Partial.Unsafe (unsafePartial)
import QueryDsl (class ConstantsToRecord, class Query, Constant(..), ParameterizedSql(..), SelectQuery, constantsToRecord, toSql)
import SQLite3 (DBConnection)
import SQLite3 as SQLite3
import Type.Row (RProxy(..))

dateTimeFormatter :: Formatter
dateTimeFormatter =
  unsafePartial $ fromJust $ hush $ parseFormatString "YYYY-MM-DDTHH:mm:ss.SSSZ"

constantToForeign :: Constant -> Aff Foreign
constantToForeign (StringConstant s) =
  pure $ unsafeToForeign s
constantToForeign (IntConstant i) =
  pure $ unsafeToForeign i
constantToForeign (NumberConstant n) =
  pure $ unsafeToForeign n
constantToForeign (DateTimeConstant dt) =
  pure $ unsafeToForeign $ format dateTimeFormatter dt
constantToForeign (OctetArrayConstant oa) =
  liftEffect $ unsafeToForeign <$> Buffer.fromArray oa
constantToForeign NullConstant =
  pure $ unsafeToForeign $ Nullable.toNullable Nothing

foreign import decodeQueryResponse
  :: forall result. U.Fn7
     (U.Fn3 String String result result)
     (U.Fn3 String Int result result)
     (U.Fn3 String Number result result)
     (EU.EffectFn3 String Buffer result result)
     (U.Fn2 String result result)
     result
     Foreign
     (Array result)

decodeQueryResponseHelper :: Foreign -> Array (Map String Constant)
decodeQueryResponseHelper =
  U.runFn7 decodeQueryResponse
    (U.mkFn3 \k v -> Map.insert k (StringConstant v))
    (U.mkFn3 \k v -> Map.insert k (IntConstant v))
    (U.mkFn3 \k v -> Map.insert k (NumberConstant v))
    (EU.mkEffectFn3 \k v m -> (\a -> Map.insert k (OctetArrayConstant a) m) <$> Buffer.toArray v)
    (U.mkFn2 \k -> Map.insert k NullConstant)
    Map.empty

runQueryInternal :: forall q. Query q => DBConnection -> q -> Aff Foreign
runQueryInternal conn q =
  case toSql q of
    Left msg ->
      throwError $ error msg
    Right (ParameterizedSql sql constants) -> do
      foreigns <- traverse constantToForeign constants
      SQLite3.queryDB conn sql foreigns

-- | Run a query and ignore any results.
runQuery :: forall q. Query q => DBConnection -> q -> Aff Unit
runQuery conn q = void $ runQueryInternal conn q

-- | Run a `SelectQuery` and return all the results.
runSelectManyQuery :: forall cols. ConstantsToRecord cols => DBConnection -> SelectQuery cols -> Aff (Array { | cols })
runSelectManyQuery conn q = do
  res <- runQueryInternal conn q
  let resMaps = decodeQueryResponseHelper res
      config = { unformatDateTime: unformat dateTimeFormatter >>> hush }
      toRecord = constantsToRecord (RProxy :: RProxy cols) config
  case traverse toRecord resMaps of
    Left msg -> throwError $ error msg
    Right recs -> pure recs

-- | Run a `SelectQuery` and either return the single result, or throw an error
-- | if there is more than one result or none at all.
runSelectOneQuery :: forall cols. ConstantsToRecord cols => DBConnection -> SelectQuery cols -> Aff { | cols }
runSelectOneQuery conn q = do
  many <- runSelectManyQuery conn q
  case many of
    [result] -> pure result
    rs -> throwError $ error $ "Expected one result, but got " <> show (Array.length rs)

-- | Run a `SelectQuery` and either return `Nothing`, if there are no results,
-- | `Just result` if there is a single result, or otherwise throw an error.
runSelectMaybeQuery :: forall cols. ConstantsToRecord cols => DBConnection -> SelectQuery cols -> Aff (Maybe { | cols })
runSelectMaybeQuery conn q = do
  many <- runSelectManyQuery conn q
  case many of
    [] -> pure Nothing
    [result] -> pure $ Just result
    rs -> throwError $ error $ "Expected one or zero results, but got " <> show (Array.length rs)
