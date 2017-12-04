module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable as U

import Test.Assert (ASSERT, assert)

collatz :: Int -> Array Int
collatz = U.unfoldr step
  where
  step 1 = Nothing
  step n =
    Just $
      Tuple n $
        if n `mod` 2 == 0
        then n / 2
        else n * 3 + 1

main :: Eff (assert :: ASSERT, console :: CONSOLE) Unit
main = do
  log "Collatz 1000"
  logShow $ collatz 1000

  log "Test none"
  assert $ U.none == [] :: Array Unit

  log "Test singleton"
  assert $ U.singleton unit == [unit]

  log "Test replicate"
  assert $ U.replicate 3 "foo" == ["foo", "foo", "foo"]

  log "Test replicateA"
  assert $ U.replicateA 3 [1,2] == [
    [1,1,1],[1,1,2], [1,2,1],[1,2,2],
    [2,1,1],[2,1,2], [2,2,1],[2,2,2]
  ]

  log "Test range"
  assert $ U.range 1 0 == []
  assert $ U.range 0 0 == [0]
  assert $ U.range 0 2 == [0, 1, 2]

  log "Test Maybe.toUnfoldable"
  assert $ U.fromMaybe (Just "a") == ["a"]
  assert $ U.fromMaybe (Nothing :: Maybe String) == []

  log "All done!"
