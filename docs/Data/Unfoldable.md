## Module Data.Unfoldable

This module provides a type class for _unfoldable functors_, i.e.
functors which support an `unfoldr` operation.

This allows us to unify various operations on arrays, lists,
sequences, etc.

#### `Unfoldable`

``` purescript
class Unfoldable t where
  unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> t a
```

This class identifies data structures which can be _unfolded_,
generalizing `unfoldr` on arrays.

The generating function `f` in `unfoldr f` in understood as follows:

- If `f b` is `Nothing`, then `unfoldr f b` should be empty.
- If `f b` is `Just (Tuple a b1)`, then `unfoldr f b` should consist of `a`
  appended to the result of `unfoldr f b1`.

##### Instances
``` purescript
instance unfoldableArray :: Unfoldable Array
```

#### `replicate`

``` purescript
replicate :: forall f a. (Unfoldable f) => Int -> a -> f a
```

Replicate a value some natural number of times.
For example:

~~~ purescript
replicate 2 "foo" == ["foo", "foo"] :: Array String
~~~

#### `replicateA`

``` purescript
replicateA :: forall m f a. (Applicative m, Unfoldable f, Traversable f) => Int -> m a -> m (f a)
```

Perform an Applicative action `n` times, and accumulate all the results.

#### `none`

``` purescript
none :: forall f a. (Unfoldable f) => f a
```

The container with no elements - unfolded with zero iterations.
For example:

~~~ purescript
none == [] :: forall a. Array a
~~~

#### `singleton`

``` purescript
singleton :: forall f a. (Unfoldable f) => a -> f a
```

Contain a single value.
For example:

~~~ purescript
singleton "foo" == ["foo"] :: Array String
~~~


