
instance Monad (Either e) where
    Right m >>= k = k m
    Left e  >>= _ = Left e
-- law 1 
--      return a >>= k
--      = Right a >>= k
--      = k a
-- law 2
--      Left a >>= return = Left a
--      
--      Right a >>= return
--      = return a
--      = Right a
-- 
-- law 3
--      Left a >>= (\x -> k x >>= h)
--      = Left a
--      = Left a >>= h
--      = (Left a >>= k) >>= h
-- 
--      Right a >>= (\x -> k x >>= h)
--      = k a >>= h
--      = (return a >>= k) >>= h
			-- by first monad law
--      = (Right a >>= k) >>= h 




instance Monad []  where
	xs >>= f = [y | x <- xs, y <- f x]
-- law 1 
--      return xs >>= k
--      = [xs] >>= k
--      = k xs
-- law 2
--      [xs] >>= return
--      = return xs
--      = [xs]
-- 
-- law 3
--      [xs] >>= (\x -> k x >>= h)
--      = k xs >>= h
--      = (return xs >>= k) >>= h
			-- by first monad law
--      = ([xs] >>= k) >>= h 


instance Monad ((->) r) where
	f >>= k = \ r -> k (f r) r
-- law 1 
--      return f >>= k
--      = (r -> f) >>= k
--      = k f
-- law 2
--      (r -> f) >>= return
--      = return f
--      = (r -> f)
-- 
-- law 3
--      (r -> f) >>= (\x -> k x >>= h)
--      = k f >>= h
--      = (return f >>= k) >>= h
			-- by first monad law
--      = ((r -> f) >>= k) >>= h 




instance Monoid a => Monad ((,) a) where
	(u, a) >>= k = case k a of
		(v, b) -> (u <> v, b)
-- law 1 
--      return b >>= k
--      = (a,b) >>= k
--      = k b
-- law 2
--      (a,b) >>= return
--      = return b
--      = (a,b)
-- 
-- law 3
--      (a,b) >>= (\x -> k x >>= h)
--      = k b >>= h
--      = (return b >>= k) >>= h
			-- by first monad law
--      = ((a,b) >>= k) >>= h 


