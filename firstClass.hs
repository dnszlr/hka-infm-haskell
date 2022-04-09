apply2 = \f -> \x -> f x

-- Für was ist das x eigentlich überhaupt da wenn apply x genau so funktioniert?
apply f x = f x

-- (\x -> x + 1) => Wird als erster Parameter an apply übergeben, der Parameter vom Aufruf von z.B. inc3 2 folgt dann darauf?
-- apply (\x -> x + 1) 2
-- apply \2 -> 2 + 1
-- apply 3
-- inc3 = 3
inc = apply (\x -> x + 1)

-- f x=2 y=3 
-- (g 1 = 1 + 2) + (g 3 = 3 + 2)
-- 3 + 5
-- 8
f x y = let g = \z -> z + x
        in (g 1) + (g y)

addLambda = \(x,y) -> x + y

------------- Curry ------------------

-- Uncurried Style
add (x,y) = x + y
addCurry = curry add

-- Curried Style --> Prefered
plus x y = x + y

plusUncurry = uncurry plus

dumb = uncurry (curry add)

