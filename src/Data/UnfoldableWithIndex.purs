module Data.UnfoldableWithIndex
  ( class UnfoldableWithIndex, unfoldrWithIndex
  , none
  , singleton
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

class UnfoldableWithIndex i f | f -> i where
  unfoldrWithIndex
    :: forall v b
     . (b -> Maybe (Tuple (Tuple i v) b))
    -> b
    -> f v

none :: forall i f a. UnfoldableWithIndex i f => f a
none = unfoldrWithIndex (const Nothing) unit

singleton :: forall i v f. UnfoldableWithIndex i f => i -> v -> f v
singleton i v = unfoldrWithIndex unfoldFn true
  where
  unfoldFn true  = Just (Tuple (Tuple i v) false)
  unfoldFn false = Nothing

-- TODO: fromMaybe
