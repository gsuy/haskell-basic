
-- instance Semigroup [a] where
--     (<>) = (++)
-- instance Monoid R where
--     mempty = []
--     mconcat xss = [ x | xs <- xss, x <- xs]


-- instance Semigroup Maybe where
--       (<>) = (+)
-- instance Monoid Maybe where
--     	mempty  = []
--     	mconcat xss = [x | xs <- xss, x <- xs]


newtype And a = And { getAnd :: a}
    deriving (Eq, Ord, Read, Show, Bounded)
instance Bool a => Monoid (And a) where
    mempty = And True
    (And a) <> (And b) = And (a && b)

newtype Or a = Or { getOr :: a}
    deriving (Eq, Ord, Read, Show, Bounded)
instance Bool a => Monoid (Or a) where
    mempty = Or False
    (Or a) <> (Or b) = Or (a || b)


maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just a) f = f a

listBind :: [a] -> (a -> [b]) -> [b]
listBind [] _ = []
listBind (x:xs) f = f x <> listBind xs f

eitherBind :: Either r a -> (a -> Either r b) -> Either r b
eitherBind (Left a) _ =  Left a
eitherBind (Right a) f =  f a

arrowBind :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
arrowBind f g = \x -> g (f x) $ x


pairBind :: (r, a) -> (a -> (r, b)) -> (r, b)
pairBind (r,a) f = f $ snd (r,a)
