
zipper (_, []) = [] --base case
zipper ([], _) = [] --base case
zipper (x:y, w:z) = (x, w): zipper (y, z) --process

-------------------------------------------------------

rev [] = [] --base case
rev x = process x [] --constructor

process [] a = a --base case
process (head:tail) arr = process tail (head:arr) --process
