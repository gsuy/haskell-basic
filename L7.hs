contain1 :: (Foldable t, Ord a) => a -> t a -> Bool
contain1 = any . (<)


contain2 :: (Foldable t, Ord a) => t a -> a -> Bool
contain2 = flip (any . (<))

len_comp [] = 0
len_comp arr = sum [ 1 | _ <- arr ]


cross _ _ [] = []
cross _ [] _ = []
cross f (x:xs) y = cross_aux x y ++ cross f xs y
    where
        cross_aux _ [] = []
        cross_aux a (b:bs)
            | f a b = (a,b) : cross_aux a bs
            | otherwise = cross_aux a bs
      