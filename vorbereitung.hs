import Data.IORef
import Test.QuickCheck

mfilter p [] = []
mfilter p (x:xs)
    | p x = x : mfilter p xs
    | otherwise = filter p xs



run x = do y <- newIORef x
           x <- square y
           -- x <- readIORef y
           print x

square x = do y <- readIORef x
              -- writeIORef x (y * y)
              return (y * y)




class Basic value where
    equalB :: value -> value -> Bool
    swapB :: value -> value
 
-- BasicValue definiert den Datentyp
data Dual = Alpha Int | Omega Int deriving Show
 
 
instance Basic Dual where
    equalB (Alpha 1) (Omega 0) = True
    equalB (Omega 1) (Alpha 0) = True
    equalB _ _ = False
 
    swapB (Alpha 0) = Omega 1
    swapB (Alpha 1) = Omega 0
    swapB (Omega 0) = Alpha 1
    swapB (Omega 1) = Alpha 0




data List value = Null | Cons value (List value) deriving Show

instance Eq a => Eq (List a) where
   Null == Null               = True
   (Cons x xs) == (Cons y ys) = x == y && xs == ys
   _ == _                     = False

listToMain Null = []
listToMain (Cons a b) = a : listToMain b

mainToList [] = Null
mainToList (x:xs) = Cons x (mainToList xs)


prop1 str = str == rev (rev str)

quick1 = quickCheck prop1

rev :: String -> String
rev [] = ""
rev str = last str : rev (skipLast str)


skipLast :: String -> String
skipLast [] = ""
skipLast [x] = ""
skipLast (x:xs) = x : skipLast xs


listToTupel list = map (\x -> (x, x * 2)) (filter (\y -> y /= 1) list)

plusLam = \x -> \y -> x + y

incLam = plusLam 1


quicksort [] = []
quicksort (x:xs) = quicksort lesser ++ [x] ++ quicksort greater
        where
            lesser = filter (<x) xs
            greater = filter (>=x) xs