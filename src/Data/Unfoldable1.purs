module Data.Unfoldable1
  ( class Unfoldable1, unfoldr1
  , replicate1
  , replicate1A
  , singleton
  , range
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Semigroup.Traversable (class Traversable1, sequence1)
import Data.Tuple (Tuple(..))

-- | This class identifies non-empty data structures which can be _unfolded_.
-- |
-- | The generating function `f` corresponds to the `uncons` operation of a
-- | non-empty list or array; it always return a value, and then optionally
-- | a value to continue unfolding from.
class Unfoldable1 t where
  unfoldr1 :: forall a b. (b -> Tuple a (Maybe b)) -> b -> t a

-- | Replicate a value `n` times. At least one value will be produced, so values
-- | `n < 1` less than one will be ignored.
-- |
-- | ``` purescript
-- | replicate1 0 "foo" == NEL.singleton "foo" :: NEL.NonEmptyList String
-- | replicate1 2 "foo" == NEL.cons "foo" (NEL.singleton "foo") :: NEL.NonEmptyList String
-- | ```
replicate1 :: forall f a. Unfoldable1 f => Int -> a -> f a
replicate1 n v = unfoldr1 step (n - 1)
  where
    step :: Int -> Tuple a (Maybe Int)
    step i
      | i <= 0 = Tuple v Nothing
      | otherwise = Tuple v (Just (i - 1))

-- | Perform an `Apply` action `n` times (at least once, so values `n < 1`
-- | less than one will be ignored), and accumulate the results.
replicate1A
  :: forall m f a
   . Apply m
  => Unfoldable1 f
  => Traversable1 f
  => Int
  -> m a
  -> m (f a)
replicate1A n m = sequence1 (replicate1 n m)

-- | Contain a single value. For example:
-- |
-- | ``` purescript
-- | singleton "foo" == NEL.singleton "foo" :: NEL.NonEmptyList String
-- | ```
singleton :: forall f a. Unfoldable1 f => a -> f a
singleton = replicate1 1

-- | Create an `Unfoldable1` containing a range of values, including both
-- | endpoints.
-- |
-- | ``` purescript
-- | range 0 0 "foo" == NEL.singleton 0 :: NEL.NonEmptyList Int
-- | range 1 2 "foo" == NEL.cons 1 (NEL.singleton 2) :: NEL.NonEmptyList Int
-- | range 2 0 "foo" == NEL.cons 2 (NEL.cons 1 (NEL.singleton 0)) :: NEL.NonEmptyList Int
-- | ```
range :: forall f. Unfoldable1 f => Int -> Int -> f Int
range start end =
  let delta = if end >= start then 1 else -1 in unfoldr1 (go delta) start
  where
    go delta i =
      let i' = i + delta
      in Tuple i (if i == end then Nothing else Just i')
