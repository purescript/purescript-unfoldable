module Data.Unfoldable.Maybe where

import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (unfoldr, class Unfoldable)
import Prelude ((<$>), flip)


-- | Convert a Maybe to any Unfoldable like lists and arrays.
toUnfoldable :: forall f a. Unfoldable f => Maybe a -> f a
toUnfoldable = unfoldr (\b -> flip Tuple Nothing <$> b)
