# Module Documentation

## Module Data.Unfoldable

### Type Classes

    class Unfoldable t where
      unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> t a


### Type Class Instances

    instance unfoldableArray :: Unfoldable Prim.Array