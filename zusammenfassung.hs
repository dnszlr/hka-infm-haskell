{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Data.IORef
import GhcPlugins (xFlags)
import GHC.Read (list)
import Data.List (sortBy)
-- Datei zusätzlich zur Prüfungsvorbereitung Sommersemester 2022 - Dennis Zeller
-- Kapitel 1: Haskell: A quick tour

-- do Befehl
main4 = do putStr "Hello "
           if 1 < 2
            then putStrLn "World"
            else do putStr "wor"
                    putStrLn "ld"

-- do Befehl
doCommand = do putStrLn "ALWAYS"
               if 1 < 2
                 then putStrLn "SOMETIMES"
                        else do putStr "NEVER"
                                putStrLn "BELIEVE"


-- Datatypes
example :: Int -> Int -> Int
example x y = x + y

example2 :: (Int -> Int) -> Int -> Int
example2 func1 value = func1 value + 12
square :: Int -> Int
square x = x * 2

-- Pure function
initValue :: Int -> Int
initValue x = x + 1

impure :: Int -> IO Int
impure x = do
    return (x+1)

-- Impure in Haskell
impure2 :: Int -> IO Int
impure2 x = do
    let output = x + 2
    return output

impure3 :: IORef Int -> IO Int
impure3 x = do
    input <- readIORef x
    let output = input + 2
    writeIORef x output
    return output

composition :: Int -> IO Int
composition = impure . initValue


-- Quicksort
quicksort [] = []
quicksort (x:xs) = quicksort lower ++ [x] ++ quicksort higher
    where
        lower = filter (<x) xs
        higher = filter (>=x) xs

-- Kapitel 2 Haskell: All about functions and lists

-- Die Funktion func kann nicht ausgeführt werden, weil x ein einzier Index einer Liste ist und xs der Tail der Liste
-- Deshalb kann sie nicht mit ++ zusammengefügt werden
-- func = (\(x:xs) -> x ++ xs)


-- Partial Application
result value = map (* value)

add x y = x + y

-- Reihenfolge Test
divide :: Integral p => p -> p -> p
divide x y = do
            if y == 0
            then 0
            else do x `div` y

divide2 x 0 = 0
divide2 x y = x `div` y

divideFloat x 0 = 0
divideFloat x y = x / y

divideBy2 :: Integer -> Integer
divideBy2 = divide 2

divideFloatBy2 :: Float -> Float
divideFloatBy2 = divideFloat 2


test123 x y = x / y


-- Function definitions
biggerThan :: Integer -> Integer
biggerThan n
    | n == 0 = 1
    | n > 10 = 11
    | otherwise = n * 2

biggerThan2 :: Integer -> Integer
biggerThan2 n =
    if n == 0
        then 1
    else if n > 10
        then 11
    else n * 2


-- Function composition
addTwo = \x -> x + 2
addThree = \x -> x + 3

funcComposition = addTwo . addThree


-- Local Definition with let
letExample :: Integral p => p -> p
letExample x =
            let a = helper x
                b = x > a
            in
            if b then
                a * 2
            else
                a `div` 2

helper x
    | x `mod` 2 == 0 && x > 2 = x + 2
    | not (x `mod` 2 == 0) && x > 2 = x - 1
    | otherwise = x + 1337

-- Local Definition with where
whereExample x =
    if b then
        a * 2
    else
        a `div` 2
    where a = helper x
          b = x > a


plus :: Num a => a -> a -> a
plus x y = x + y
-- plus 1 2 = (plus 1) 2
-- Result 3 



-- Higher-order function und first class functions

apply f x = f x
inc3 = apply (\x -> 1 + x)

f x y = let g = \z -> z + x
        in (g 1) + (g y)


-- Curry / Uncurry
curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x -> \y -> f (x,y)
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \(x,y) -> f x y

addCurry x y = x + y
addUncurry (x,y) = x + y

-- Listen

liste = [1, 2, 3, 4]
stringListe = [['S', 't', 'r'], ['i', 'n', 'g']]

-- Append element at the end of a list
attach :: Int -> [Int] -> [Int]
attach num [] = [num]
attach num (x:xs) = [x] ++ attach num xs

elemCheckFalse = 22 `elem` [1,2,3]
elemCheckTrue = 1 `elem` [1,2,3]

nullTrue = null []
nullFalse = null [1, 2, 3]

-- List Sum Up
sumUp [] = 0
sumUp(x:xs) = x + sumUp xs

sumUp2 :: Num p => [p] -> p
sumUp2 list
    | null list = 0
    | otherwise = head list + sumUp2 (tail list)


bottom [x] = x
bottom (x:xs) = bottom xs

-- Pattern Matching with blanked 

blank _ _ = 0

isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

myDiv n 0 = error "division by zero"
myDiv n m = n `div` m

myAnd False _ = False
myAnd True x  = x


myRealAnd True True = True
myRealAnd _ _ = False


-- Map

letMap =
    let sumSquare = \x -> (x + x) ^ 2
    in map sumSquare

whereMap = map getHalf
    where getHalf x = x * 0.5


lambdaMap = map (\x -> x `mod` 2 + x ^ 2)

-- Filter

greater x = filter (>x)

boolCheck bool list= filter (==True) list

boolCheckLambda bool = filter (\x -> x == bool)

letFilter =
    let a = (> 10)
    in filter a

whereFilter = filter squareGreaterTen
                where squareGreaterTen x = x ^ 2 > 10


-- list comprehensions

comprehensionSquareIfEven xs = [x ^ 2 | x <- xs, x `mod` 2 == 0]
-- Fügt Funktion auf neue Liste durch | Erzeug neue Liste, bestimmt was in die neue Liste kommt
-- comprehensionSquareIfEven [1,2,3,4]
-- x mod 2 == 0 -> False,True, False,True ==> [2,4]
-- x = [2,4]
-- 2 ^ 2 = 4, 4 ^ 2 = 16
-- neue Liste == [4, 16]


flatten2 xss = [x | xs <- xss, x <- xs]


-- foldr f z []     = z
-- foldr f z (x:xs) = f x (foldr f z xs)
-- foldr   (+) 1 [1,2,3] =
-- foldr = (+) 1 (foldr (+) 1 [2,3])
-- foldr = (+) 2 (foldr (+) 1 [3])
-- foldr = (+) 3 (foldr (+) 1 [])  
-- foldr = 1 -- Leere Liste übergeben			      
-- recursive return
-- foldr = 1
-- foldr = (+) 3 1 = 4
-- foldr = (+) 2 4 = 6
-- foldr = (+) 1 6 = 7
-- Ergebnis = 7

-- foldl f z []     = z
-- foldl f z (x:xs) = foldl f (f z x) xs
-- foldl   (+) 1 [1,2,3] =
-- foldl = foldl (+) ((+) 1 1) [2,3]
-- foldl = foldl (+) ((+) 2 2) [3]
-- foldl = foldl (+) ((+) 4 3) []
-- foldl = 7 -- Leere Liste übergeben z = 7
-- Ergebnis = 7, direkt Rekursiv nach oben gegeben




resultTrue = or [True, False, False, False]
resultFalse = or [False, False, False, False]


anyTrue = any (>1) [1,2,4,6]
anyFalse = any even [1,3,5,7]

-- DB Example

-------------------------------------------------------
-- Extended Haskell programming exercise
-- Topic: functions over lists
-- Author: Martin Sulzmann
-------------------------------------------------------

-- A student is represented by her name, student id and a list of the courses the student is taking
type Student = (String, Int, [Int])
type DB = [Student]

-- TASK 0
{-
Databases must be consistent.
We say the database is consistent if there're no multiple entries
of students, and no multiple entries of courses per students
 For example, the below databases are *inconsistent*
-}

-- Getter
getIds :: DB -> [Int]
getIds = map(\(_,id,_) -> id)


getId2 = \(_,id,_) -> id

getterId :: Student -> Int
getterId (_,id,_) = id

getNames :: DB -> [String]
getNames = map(\(name,_,_) -> name)

getterName :: Student -> String
getterName (name,_,_) = name

getCourses :: DB -> [[Int]]
getCourses = map(\(_,_,courses) -> courses)

getterCourses :: Student -> [Int]
getterCourses (_,_,courses) = courses

cons :: [([Char], Int, [Int])]
cons = [("Dennis", 1, [141,1337]), ("Gianni", 2, [141,252]), ("Philipp", 3, [141,252,1337])]

incons1 :: DB
incons1 = [("Jack", 111, [141, 252, 141])]
incons2 :: DB
incons2 = [("Jane", 112, [141, 252]), ("Jane", 112, [141, 252])]

{-
Your task is to implement the following function
which returns True if the database is valid (consistent),
otherwise the function returns False.
-}
valid :: DB -> Bool
valid db = validA db && validB db && validC db

-- No multiple entries of students
validA :: DB -> Bool
validA [] = True
validA [student] = True
validA db = noDups db

-- No multiple entries of courses per student
validB :: DB -> Bool
validB [] = False
validB db = and (map noDups (getCourses db))

noDups [] = True
noDups (x:xs)
    | x `elem` xs = False
    | otherwise = noDups xs


-- EXTENSION TO TASK 0
{-
Extension: We strengthen the notion of consistency.
In addition, we require that there shall by no duplicate student id's.
For example,
-}

incons3 :: [([Char], Int, [Int])]
incons3 = [("Jane", 111, [141]), ("Jack", 111, [141, 252])]

validC :: DB -> Bool
validC db = noDups (getIds db)
-- FROM HERE ON, WE WILL ASSUME THAT ALL DATABASES ARE CONSISTENT !!!


-- TASK 1
{-
Given a database and a student id, we're looking for the list of
courses of this particular student.
-- DB nach ID durchsuchen
-- Schauen ob student id == id
-- Student holen
-- Kurse ausgeben
-}
query1 :: DB -> Int -> [Int]
query1 db id = let students = getStudentById db id
               in
                if length students == 1
                then head (map (\(_,_,course) -> course) students)
                else []


getStudentById :: DB -> Int -> [Student]
getStudentById db id = filter (\(_,sid,_) -> id == sid) db

-- TASK 2
{-
Given a database and a course, find all students
taking this course.
-- Alle Studenten die in einem Kurs sind
-- Namen ausgeben
-}
query2 :: DB -> Int -> [String]
query2 db course = map (\(name,_,_) -> name) (filter (\(_,_,courses) -> course `elem` courses) db)

-- TASK 3
{-
Given a database, sort the database (in non-decreasing order)
according to the name of students.
-}
sortDB :: DB -> DB
sortDB db = sortBy (\(s1, _, _) -> \(s2, _, _) -> 
                    if s1 == s2 then EQ
                    else if s1 < s2 then LT
                    else GT) db


customSort list = sortBy compare list

{-
Extension1:
Provide a function sortDB' which sorts the database according to the number of courses a student is taking
-}
sortDBByAmountOfCourse :: DB -> DB
sortDBByAmountOfCourse db = sortBy(\(_,_,courses1) -> \(_,_,courses2) ->
                            let lc1 = length courses1
                                lc2 = length courses2
                            in
                            if lc1 == lc2 then EQ 
                            else if lc1 < lc2 then LT 
                            else GT) db
{-
Extension2:
Provide a function sortDB'' which is parameterized in the actual comparing relation which determines the sorting order
Then you can define
sortDB = sortDB'' cmpName

-}

cmpName :: Student -> Student -> Ordering
cmpName (n1, _, _) (n2, _, _) =
    if n1 == n2 then EQ
    else if n1 > n2 then LT
    else GT


sortDB'' = sortBy


-- TASK 4
{-
Given two databases, merge them to obtain one (consistent) database
 Example:

 merge [("Jane", 112, [141, 353])] [("Jane", 112, [141, 252])]
    => [("Jane", 112, [141, 252, 353])]

-}

mergeDB1 :: DB
mergeDB1 = [("Jane", 112, [141, 353])]
mergeDB2 :: DB
mergeDB2 = [("Jane", 112, [141, 252])]
mergeDB3 :: DB
mergeDB3 = [("Jane", 112, [141, 353])] 
mergeDB4 :: DB
mergeDB4 =[("Jane", 113, [141, 252])]

merge :: DB -> DB -> DB
merge db1 db2 = merge' (sortDB db1) (sortDB db2)
                where
                  merge' [] db = db
                  merge' db [] = db
                  merge' ((n1,i1,c1):db1) ((n2,i2,c2):db2)
                    | n1 == n2   = (n1,i1,rmDups (c1++c2)) : merge' db1 db2
                    | n1 < n2    = (n1,i1,c1) : merge' db1 ((n2,i2,c2):db2)
                    | n1 > n2    = (n2,i2,c2) : merge' ((n1,i1,c1):db1) db2


rmDups xs = rmDups' [] xs
rmDups' acc []     = acc
rmDups' acc (x:xs)
  | elem x acc = rmDups' acc xs
  | otherwise  = rmDups' (x:acc) xs


-- Kapitel 3: Data Types und User-Controlled Overloading

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun 

printWD :: Weekday -> String 
printWD Mon = "Monday"
printWD Tue = "Tuesday"
printWD Wed = "Wednesday"
printWD Thu = "Thursday"
printWD Fri = "Friday"
printWD Sat = "Saturday"
printWD Sun = "Sunday"

-- Argumente für Konstruktoren
data Result = OK Int | Failure String deriving (Show, Eq, Ord)

divZero a b
    | b == 0 = Failure "Div by Zero"
    | otherwise = OK (a `div` b)


data Maybe a = CJust a | CNothing deriving (Show, Eq, Ord)

divZeroMaybe a b =
    if b == 0 then CNothing
    else CJust (a `div` b)

data Either a b = CLeft a | CRight b deriving (Show, Eq, Ord)

bigger a b 
    | a < b = CLeft (b)
    | a >= b = CRight (a)

-- Datenstrukturen

