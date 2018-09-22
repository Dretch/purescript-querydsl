module Test.Main where

import Prelude

import Effect (Effect)
import Test.QueryDsl as QueryDsl
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  QueryDsl.test
