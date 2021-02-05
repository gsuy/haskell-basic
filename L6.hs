
filter_concat f arr = filter_concat_aux arr []
    where
        filter_concat_aux [] res = res
        filter_concat_aux (x:xs) res
            | f x = filter_concat_aux xs (res ++ x)
            | otherwise = filter_concat_aux xs res


take_while f arr = take_while' arr []
    where
        take_while' [] res = reverse res
        take_while' (x:xs) res
            | f x = take_while' xs (x:res)
            | otherwise = reverse res


filter_concat'' f [] = []
filter_concat'' f x = concat $ filter f x