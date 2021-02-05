partition p [] = ([], [])
partition p (x:xs)
  | p x       = (x:l, r)
  | otherwise = (l, x:r)
  where (l, r) = partition p xs

filter' f list = fst (partition f list)

qsort [] = []
qsort (hd:tl) = qsort l ++ [hd] ++ qsort r
  where l = fst (partition (<hd) tl)
        r = fst (partition (>=hd) tl)