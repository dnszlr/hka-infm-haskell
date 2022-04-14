
-- Notes for Test1 and Try1 on 03.04.2022

-- Aufgabe 1: How to sum up 1 and 2 via function plus?
-- plus x y = x + y
-- Antwort (plus 1) 2

-- Aufgabe 2: Remove as many parantheses as possible in function g. --> Nicht richtig kapiert

-- curry2 :: ((a, b) -> c) -> a -> b -> c
-- curry2 f = \x -> \y -> f (x,y)
-- uncurry2 :: (a -> b -> c) -> (a, b) -> c
-- uncurry2 f = \(x,y) -> f x y

-- f = curry2 . uncurry2
-- g = f plus 2 3

-- Aufgabe 3: Which of the parameters is a function? --> Nicht kapiert

--ff x = \f -> \y -> f y x

-- Aufgabe 4: Define function inc in terms of plus. 

--plus x y = x + y
--inc = plus 1

-- Aufgabe 5:  Remove as many parantheses as possible in function plus2.

--plus2 = \x -> (\y -> x + y)

-- Aufgabe 6: Which of the following holds?

--ff x = \f -> \y -> f y x

--g1 = ff 1 plus

-- Aufgabe 7: Define plus via a combination of the above functions.

-- curry and uncurry are pre-defined, we repeat their definitions introducing
-- new names to avoid clashes

--curry2 :: ((a, b) -> c) -> a -> b -> c
--curry2 f = \x -> \y -> f (x,y)
--uncurry2 :: (a -> b -> c) -> (a, b) -> c
--uncurry2 f = \(x,y) -> f x y

--add (x,y) = x + y


-- Aufgabe 8: What is the result of evaluating g?
--curry2 :: ((a, b) -> c) -> a -> b -> c
--curry2 f = \x -> \y -> f (x,y)
--uncurry2 :: (a -> b -> c) -> (a, b) -> c
--uncurry2 f = \(x,y) -> f x y

--f = curry2 . uncurry2
--g = ((f plus) 2) 3