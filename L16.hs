maybeAp :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeAp Nothing _ = Nothing
maybeAp _ Nothing = Nothing
maybeAp (Just a) (Just b) = Just $ a b

initMaybe :: a -> Maybe a
initMaybe a = Just a

listAp :: [a -> b] -> [a] -> [b]
listAp [] _ = []
listAp _ [] = []
listAp fl la = [f l | f <- fl, l <- la]

initList :: a -> [a]
initList a = [a]