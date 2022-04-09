import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.IORef
main = putStrLn "Hello World"

main2 = do putStr "Hello "
           putStrLn "World"

-- Fehler da nicht korrekt eingerückt
--main2' = do putStr "Hello "
--          putStrLn "World" -- commands are not aligned!

main5 = do
    print "What is your name?"
    name <- getLine
    print ("Hello " ++ name ++ "!")


imperativ = do
    x <- newIORef (1::Int)
    v <- readIORef x
    writeIORef x (v + 1)
    print "Do something"

-- Pure function
inc1 :: Int -> Int
inc1 x = x + 1

-- Impure function as signalled by the return type IO Int
inc2 :: Int -> IO Int
inc2 x = do
   print "Hello"
   return (x + 1)

-- Yet again an impure function where the input is a reference
inc3 :: IORef Int -> IO Int
inc3 x = do
  v <- readIORef x
  let v2 = v + 1
  writeIORef x v2
  print "Test"
  return v2

test = do
    r <- newIORef 2
    t <- inc3 r
    print t  

compose1 :: Int -> Int
compose1 = inc1 . inc1 . inc1

compose2 :: Int -> IO Int
compose2 = inc2 . inc1

-- Will NOT type check.
-- Cannot mix Int with IO Int!
-- compose2b = inc1 . inc2