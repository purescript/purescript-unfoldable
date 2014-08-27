module Data.Unfoldable where

import Data.Maybe
import Data.Tuple
import Control.Monad.Eff
import Control.Monad.ST

class Unfoldable t where
  unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> t a
 
foreign import newEmptySTArray 
  "function newEmptySTArray() {\
  \  return [];\
  \}" :: forall eff h a. Eff (st :: ST h | eff) (STArray h a)

instance unfoldableArray :: Unfoldable [] where
  unfoldr f b = runPure (runSTArray (do
    arr  <- newEmptySTArray
    seed <- newSTRef b
    idx  <- newSTRef 0
    untilE $ do
      b1 <- readSTRef seed
      case f b1 of
        Nothing -> return true
        Just (Tuple a b2) -> do
          i <- readSTRef idx
          pokeSTArray arr i a
          writeSTRef seed b2
          writeSTRef idx (i + 1)
          return false
    return arr))
