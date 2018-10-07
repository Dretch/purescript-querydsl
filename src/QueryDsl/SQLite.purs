module QueryDsl.SQLite (
  runQuery,
  runSelectManyQuery,
  runSelectOneQuery
) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn6, runFn6)
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (traverse)
import Effect.Aff (Aff, error, throwError)
import Foreign (Foreign)
import QueryDsl (class ConstantsToRecord, class Query, Constant(..), ParameterizedSql(..), SelectQuery, constantsToRecord, toSql)
import SQLite3 (DBConnection)
import SQLite3 as SQLite3
import Type.Row (RProxy(..))

paramToString :: Constant -> String
paramToString (StringConstant s) = s
paramToString (IntConstant i) = show i
paramToString (NumberConstant n) = show n
paramToString NullConstant = ""

foreign import decodeQueryResponse
  :: Fn6
     (String -> String -> Map String Constant -> Map String Constant)
     (String -> Int -> Map String Constant -> Map String Constant)
     (String -> Number -> Map String Constant -> Map String Constant)
     (String -> Map String Constant -> Map String Constant)
     (Map String Constant)
     Foreign
     (Array (Map String Constant))

decodeQueryResponseHelper :: Foreign -> Array (Map String Constant)
decodeQueryResponseHelper =
  runFn6 decodeQueryResponse
    (\k v -> Map.insert k (StringConstant v))
    (\k v -> Map.insert k (IntConstant v))
    (\k v -> Map.insert k (NumberConstant v))
    (\k -> Map.insert k NullConstant)
    Map.empty

runQueryInternal :: forall q. Query q => DBConnection -> q -> Aff Foreign
runQueryInternal conn q =
  case toSql q of
    Left msg ->
      throwError $ error msg
    Right (ParameterizedSql sql params) ->
      SQLite3.queryDB conn sql (paramToString <$> params)

-- | Run a query and ignore any results.
runQuery :: forall q. Query q => DBConnection -> q -> Aff Unit
runQuery conn q = void $ runQueryInternal conn q

-- | Run a `SelectQuery` and return all the results.
runSelectManyQuery :: forall cols a. ConstantsToRecord cols => DBConnection -> SelectQuery cols a -> Aff (Array { | cols })
runSelectManyQuery conn q = do
  res <- runQueryInternal conn q
  let resMaps = decodeQueryResponseHelper res
      toRecord = constantsToRecord (RProxy :: RProxy cols)
  case traverse toRecord resMaps of
    Left msg -> throwError $ error msg
    Right recs -> pure recs

-- | Run a `SelectQuery` and either return the single result, or throw an error
-- | if there is more than one result or none at all.
runSelectOneQuery :: forall cols a. ConstantsToRecord cols => DBConnection -> SelectQuery cols a -> Aff { | cols }
runSelectOneQuery conn q = do
  many <- runSelectManyQuery conn q
  case many of
    [result] -> pure result
    rs -> throwError $ error $ "Expected one result, but got " <> show (Array.length rs)
