add i = i + i
multiplicate i = i * i

alwaysEvenLet a b = let isEven x = if even x
                                   then x
                                   else x - 1
                                   in (isEven a, isEven b)

alwaysEvenWhere a b = (isEven a, isEven b)
 where isEven x = if even x then x else x - 1

quicksort [] = []
quicksort (p:xs) = quicksort lesser ++ [p] ++ quicksort greater
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

tester temp = (temp * temp) * 2

plus3 = \x -> (\y -> x + y)