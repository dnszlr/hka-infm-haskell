import Data.IORef
import Test.QuickCheck

mfilter p [] = []
mfilter p (x:xs)
    | p x = x : mfilter p xs
    | otherwise = filter p xs

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
myHead [x] = x
myHead(x:xs) = x


sumUp :: Num a => [a] -> a
sumUp [] = 0
sumUp[x] = x
sumUp(x:xs) = x + sumUp xs

myFilter p [] = []
myFilter p (x:xs)
    | p x = x : myFilter p xs
    | otherwise = myFilter p xs


add :: (Int,Int) -> Int
add (x,y) = x + y
curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f = \x -> \y -> f (x,y)

plus :: Int -> Int -> Int
plus = curry2 add

flatten2 xss = [x | xs <- xss, x <- xs]


countSingle [] = 0
countSingle (x:xs)
    | length x == 1 = 1 + countSingle xs
    | otherwise = countSingle xs


getName1 (name,_,_) = name

getName2 = \(name,_,_) -> name

getNames = map (\(name,_,_) -> name)

type ProductName = String
type ProductPrice = Float
type Product = (ProductName, Int, ProductPrice)
type ProductList = [Product]

prodA =  ("Beer", 200, 2.50)
prodB = ("Milk", 200, 3.50)
prodC = ("Coffee", 300, 8.50)
prodD = ("Tea", 100, 1.50)

prodList :: ProductList
prodList = [prodA, prodB]

productCost::Product -> ProductPrice
productCost (_, amount, price) = fromIntegral amount * price

lessEqual :: Product -> Product -> Bool
lessEqual (_,amountA, priceA) (_,amountB, priceB)
    | amountA == amountB = True
    | amountA < amountB && priceA > priceB = False
    | otherwise = False

overalCost [] = 0
overalCost [x] = productCost x
overalCost (x:xs) = productCost x + overalCost xs

prodAA =  ("Beer", 1, 2.50)
prodBB = ("Milk", 0, 3.50)
prodCC = ("Coffee", 2, 8.50)
prodDD = ("Tea", 2, 1.50)

prodList2 :: ProductList
prodList2 = [prodAA, prodBB, prodCC, prodDD]


decProduct :: ProductList -> ProductName -> (ProductList, ProductList)
decProduct xs name = let dec = decStep xs name
                     in (dec, isEmpty dec)


decStep :: ProductList -> ProductName -> ProductList
decStep [] nameS = []
decStep xs nameS = let (name,amount,price) = head xs
                     in if nameS == name
                     then (name,amount -1, price) : decStep (tail xs) nameS
                     else (name, amount, price) : decStep (tail xs) nameS

isEmpty :: ProductList -> ProductList
isEmpty [] = []
isEmpty (x:xs) = let (_,amount,_) = x
                 in if amount == 0 then x : isEmpty xs
                 else isEmpty xs


prodAAA :: Product
prodAAA =  ("Beer", 99, 2.50)
prodBBB :: Product
prodBBB = ("Milk", 0, 3.50)
prodCCC :: Product
prodCCC = ("Beer", 1, 8.50)
prodDDD :: Product
prodDDD = ("Tea", 2, 1.50)

list1 :: ProductList
list1 = [prodAAA, prodBBB]
list2 :: ProductList
list2 = [prodCCC, prodDDD]

combineProductList :: ProductList -> ProductList -> ProductList
combineProductList [] ys = ys
combineProductList xs [] = xs
combineProductList (x:xs) ys = if getName x `elem` getNames ys then merge x (getProductByName (getName x) ys) : combineProductList xs ys
                               else x : combineProductList xs ys


merge x y = let (name1, amount1, price1) = x
                (name2, amount2, price2) = y
            in if price1 >= price2 then (name1, amount1 + amount2, price1)
            else (name2, amount1 + amount2, price2)


getProductByName name ys = if getName (head ys) == name then head ys
                           else getProductByName name (tail ys)

getName (name,_,_) = name


type Table = [(Char, [Code])]
data Code = Zero | One deriving (Eq,Show)


code1 = [One, Zero, One, Zero]
code2 = [Zero, Zero, One, Zero]

table1 = [('a', code1), ('b', code2)]


retrieve:: Table -> Char -> [Code]
retrieve (x:xs) char
    | getCharrr x == char = getByte x
    | otherwise = retrieve xs char

getCharrr (char,_) = char

getByte (_,byte) = byte



headOwn :: [x] -> x
headOwn [x] = x
headOwn (x:xs) = x

plus2 :: Int -> Int -> Int 
plus2 = curry2 add

myFlatten xss = [x | xs <- xss, x <- xs]


count [] = 0
count xs
    | length (head xs) == 1 = 1 + count (tail xs)
    | otherwise = count (tail xs)


summit Null = 0
summit (Cons a b) = a + summit b



class Type value where
    adds :: value -> value -> value
    addss :: value -> value -> value

data OK = Oki Int | Oko Int deriving Show

instance Type OK where
    adds (Oki 1) (Oki 2) = (Oko 3)
    addss (Oki a) (Oki b) = Oko (a + b)



data Weekend = Saturday | Sunday

instance Show Weekend where
    show = showWeekend

instance Num Weekend where
    (+) = addWeekend


showWeekend :: Weekend -> String
showWeekend Saturday = "Samstag"
showWeekend Sunday = "Sonntag"


addWeekend Saturday Saturday = Sunday
addWeekend Sunday Sunday = Saturday



run :: Int -> IO ()
run x = do y <- newIORef x
           output <- square y
           -- square y
           --output <- readIORef y
           print output

square x = do y <- readIORef x
              return (y * y)
              --writeIORef x (y * y)


nuull (x:xs) = False
nuull [] = True


test list = map(\x -> x) (filter (\y -> y /= 1) list)
