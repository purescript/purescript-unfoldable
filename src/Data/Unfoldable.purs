module Data.Unfoldable where

import Data.Maybe
import Data.Tuple
import Data.Function

class Unfoldable t where
  unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> t a
 
foreign import unfoldrArray 
  "function unfoldrArray(f, b) {\
  \  var result = [];\
  \  while (true) {\
  \    var maybe = f(b);\
  \    if (maybe.ctor === \"Data.Maybe.Nothing\") {\
  \      return result;\
  \    } else if (maybe.ctor === \"Data.Maybe.Just\") {\
  \      result.push(maybe.values[0].values[0]);\
  \      b = maybe.values[0].values[1];\
  \    }\
  \  }\
  \}" :: forall a b. Fn2 (b -> Maybe (Tuple a b)) b [a]

instance unfoldableArray :: Unfoldable [] where
  unfoldr = runFn2 unfoldrArray
