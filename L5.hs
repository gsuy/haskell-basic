list_map f a = run a []
    where
        run [] l = rev l
        run (x:xs) l = run xs (f x :l)

zipper a b = zipp a b []
    where
        zipp [] _ l = rev l
        zipp _ [] l = rev l
        zipp (x:xs) (y:ys) l = zipp xs ys ((x,y):l)


