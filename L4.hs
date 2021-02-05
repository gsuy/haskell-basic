rev l = rev_aux l []
    where
        rev_aux [] res = res
        rev_aux (x:xs) res = rev_aux xs (x:res) 

fib :: Integral p => p -> p
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


