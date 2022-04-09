isEven :: Integer -> Bool
isEven n
    | mod n 2 == 0 = True 
    | otherwise  = False


isEven2 :: Integer -> Bool
isEven2 n =
   if n `mod` 2 == 0
   then True
   else False

-- Given functions f and g, we can define f . g as the composition where (f . g) x is equivalent to f (g (x))
-- not(isEven(2)) => False
isOdd :: Integer -> Bool
isOdd = not . isEven

plus x y = x + y
-- (plus 1) inc parameter = 1 + inc parameter
-- Left-associative for functions means that the left side of the function call (plus x) y is applied first, this results in "(plus (plus x) y) = plus x y"
inc = plus 1

-- lambda = function without a name

basicInc x = x + 1

-- \ ist eigentlich einfach der anoynme Name der Funktion.
basicIncLambda = \x -> x + 1

shortInc = (+1)

plusDualLambda :: Integer -> Integer -> Integer
plusDualLambda = \x -> \y -> x + y

-- ist das selbe wie \y -> 2 + y
-- wird also aufgerufen mit beispielsweise hardcoreInc 14: 14 -> 2 + 14 = 16
hardcoreInc = plusDualLambda 2