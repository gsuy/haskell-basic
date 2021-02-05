
concat' l = foldl (++) [] l
concat'' l = foldr (++) [] l

reverse_left [] = []
reverse_left l = foldl (\x -> (:x)) [] l


reverse_right [] = []
reverse_right l = foldr (\x -> (++[x])) [] l


map_fold _ [] = []
map_fold f l = foldr ((:) . f) [] l


filter_fold _ [] = []
filter_fold f l = foldr (\x y -> if f x then x:y else y) [] l

ex _ [] = True
ex f l = foldl (\_ x -> if f x then True else False) True l

elem' _ [] = False
elem' input l = foldl (\acc x -> if input==x then True else (if acc==True then True else False)) False l
