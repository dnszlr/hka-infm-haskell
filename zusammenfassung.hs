import Data.IORef
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
 