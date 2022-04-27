-- (a,a) -> a
add (x,y) = x + y
-- a -> (a -> a)
add2 x y = x + y


quicksort [] = []
quicksort(x:xs) = quicksort lesser ++ [x] ++ quicksort greater
    where
        lesser = filter (<x) xs
        greater = filter(>= x) xs