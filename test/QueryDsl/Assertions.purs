module Test.QueryDsl.Assertions (shouldBeSql) where

import Prelude (Unit)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import QueryDsl (ParameterizedSql)
import Test.Spec.Assertions (shouldEqual)

shouldBeSql :: Either String ParameterizedSql -> ParameterizedSql -> Aff Unit
shouldBeSql actual expected = actual `shouldEqual` Right expected
