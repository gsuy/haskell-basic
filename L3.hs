
zipper (_, []) = [] --base case
zipper ([], _) = [] --base case
zipper (x:y, w:z) = (x, w): zipper (y, z) --process

zipper' = \x y -> zipper (x,y) --curried version of zipper

fac 0 = 1
fac n = n * fac (n-1)

