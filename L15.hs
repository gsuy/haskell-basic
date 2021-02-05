class Functor' f where
    fmap :: (a -> b) -> f a -> f b

    
instance Functor' ((->) r) where
    fmap f g = f . g
