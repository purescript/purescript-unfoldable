module Test.Main where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console
import Data.Maybe
import Data.Unfoldable
import Test.Assert

main = do
  log "Test none"
  assert $ none == [] :: Array Unit

  log "Test singleton"
  assert $ singleton unit == [unit]

  log "Test replicate"
  assert $ replicate 3 "foo" == ["foo", "foo", "foo"]

  log "Test replicateA"
  assert $ replicateA 3 [1,2] == [
    [1,1,1],[1,1,2], [1,2,1],[1,2,2],
    [2,1,1],[2,1,2], [2,2,1],[2,2,2]
  ]

  log "All done!"
