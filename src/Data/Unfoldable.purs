module Data.Unfoldable where

import Data.Maybe
import Data.Tuple
import Data.Array.ST
import Control.Monad.Eff
import Control.Monad.ST

class Unfoldable t where
  unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> t a
 
instance unfoldableArray :: Unfoldable [] where
  unfoldr f b = runPure (runSTArray (do
    arr  <- emptySTArray
    seed <- newSTRef b
    untilE $ do
      b1 <- readSTRef seed
      case f b1 of
        Nothing -> return true
        Just (Tuple a b2) -> do
          pushSTArray arr a
          writeSTRef seed b2
          return false
    return arr))
