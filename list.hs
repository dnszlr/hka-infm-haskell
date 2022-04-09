-- preperation for test 2, programming paradigms on 08.04.2022
list :: [Integer]
list = [1,2,3,4,5]

listlist :: [[Char]]
listlist = [['a','b','c'], ['d','e'], [] , ['f','g']]

stringList :: [[Char]]
stringList = ["Hallo", ['w', 'o', 'r'], ['l'], [], ['d']]

-- Add 1 to each list element
addUp :: [Integer] -> [Integer]
addUp = map (+1)

-- Append value to list
combine :: a -> [a] -> [a]
combine value list = value : list
-- short version to append values to a list
combineShort :: a -> [a] -> [a]
combineShort = (:)

plus :: Num a => a -> a -> a
plus a b = a + b
plusShort :: Integer -> Integer -> Integer
plusShort = (+)

-- concat two lists with each other
concatList :: [a] -> [a] -> [a]
concatList xs1 xs2 = xs1 ++ xs2
concatListShort :: [a] -> [a] -> [a]
concatListShort = (++)

-- AND = Alle Indizes sind wahr, OR = Mindestens ein Index ist wahr
-- or = True, and = False
boolListMix :: [Bool]
boolListMix = [True, False]

-- or = True, and = True
boolListTrue :: [Bool]
boolListTrue = [True, True]

-- or = False, and = False
boolListFalse :: [Bool]
boolListFalse = [False, False]

-- head [1] = 1, tail [1] = empty list ==> tail lässt einfach das erste Element der Liste fallen.
-- sum up all list values (equivalent to sum ()...)
sumUp :: Num p => [p] -> p
sumUp list
    | null list = 0
    | otherwise = head list + sumUp(tail list)

sumUp2 :: Num p => [p] -> p
sumUp2 = 
    \xs -> sum xs

sumUpPattern :: Num a => [a] -> a
sumUpPattern [] = 0
sumUpPattern [x] = x
sumUpPattern [x, y] = x + y
sumUpPattern (x:xs) = x + sumUpPattern xs

-- AND function implementation with pattern matching
myAnd :: Bool -> Bool -> Bool
myAnd False _ = False 
myAnd True x = x

-- Get last element version 1
lastElement [] = 25 -- lel
lastElement [x] = x
lastElement (x:xs) = lastElement xs

-- Get last element version 2
lastElement2 :: Num a => [a] -> a
lastElement2 [x] = x
lastElement2 [x,y] = y
lastElement2 list
    | length list == 1 = head list
    | otherwise = lastElement2 (tail list)

sumUpWithTail [] = 0
sumUpWithTail list = sum list + sumUpWithTail (tail list)

-- Map
map1 :: [Integer] -> [Integer]
-- test [1,2,3]
-- map 1 -> 1 + 1
-- map 2 -> 2 + 2
-- map 3 -> 3 + 3
map1 = map(\n -> n + n)


map2 :: [Int] -> [Int]
map2 = map( \n -> let element = n
                   in 
                       if even element
                       then element * 2
                       else element * 100)

-- 2,4,2,8,2,10,2,12,2,16

-- filter

filter1 = filter (\n -> n * 2 < 5)


-- list comprehensions
-- [Map | Iterator, Filter]
-- [f x | x <- xs, p x]

comprehension1 xs = [x * 2 | x <- xs, x / 2 > 10]
-- Schrittfolge [2, 1, 3]
-- Zuerst Iteration (Erzeugt Liste?) Im Endeffekt sagt vor | was getan werden soll und nach | bestimmt die Liste die erzeugt werden sollen
-- Dann Filter --> Erzeugt neue Liste mit Elementen die dem Filter entsprechen
-- Dann Mapper --> Führt auf die Filter Liste die gewünschte Mappen Funktion aus