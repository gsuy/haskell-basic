
elem' _ [] = False
elem' input l = run l False
    where
        run _ True = True
        run [] res = res
        run (x:xs) _ = run xs (x==input)

elem'' _ [] = False
elem'' input l = foldl (\acc x -> if input==x then True else (if acc==True then True else False)) False l


partition' p [] = ([],[])
partition' p l = foldl (\(a,b) x -> if p x then (a++[x],b) else (a,b++[x])) ([],[]) l



foldl' f a l = foldr (flip f) a (reverse l)

foldr' f a l = foldl (flip f) a (reverse l)



