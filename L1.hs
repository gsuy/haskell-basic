join [] base = base
join (x:y) z = x : join y z
