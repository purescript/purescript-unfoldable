# Module Documentation

## Module Data.Unfoldable

#### `Unfoldable`

``` purescript
class Unfoldable t where
  unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> t a
```


#### `unfoldableArray`

``` purescript
instance unfoldableArray :: Unfoldable Prim.Array
```