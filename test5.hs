import System.Random
import Data.Char
import Test.QuickCheck


-- Frage 1

-- tqc1

-- Consider the following property which
-- states that reversing a string twice yields the original string.


prop_tqc1 :: String -> Bool
prop_tqc1 xs = reverse (reverse xs) == xs

tqc1 = quickCheck prop_tqc1

{-

 What is the result reported by quickCheck?
 Load the above program text into ghci and evaluate tqc1.

 A: Ok

 B: Failed

-}

-- Frage 2

-- tqc5

-- Consider

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)


-- We wish to provide for an Arbitrary instance for trees.

{-

Here's a sketch. Some code parts are omitted, see @AAA@ and @BBB.

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = do left <- arbitrary
                             right <- @AAA@
                             x <- arbitrary
                             let y = [Leaf x, Node left right]
                             r <- @BBB@ y
                             return r


Which of the following code completions yield a functioning Arbitrary instance for trees?


  A:   Replace @AAA@ by arbitrary.
       Replace @BBB@ by elements.

  B:   Replace @AAA@ by arbitrary.
       Replace @BBB@ by arbitrary.

  C:   Replace @AAA@ by elements.
       Replace @BBB@ by elements.
-}

-- Frage 3

-- tqc2

-- Consider the following property which
-- states that reversing a string twice yields the original string.
-- The actual reverse function is a parameter and
-- will be supplied when calling quickCheck.

prop_tqc2 :: (String -> String) -> String -> Bool
prop_tqc2 rev xs = rev (rev xs) == xs

myRev :: String -> String
myRev [x,y] = [y,x]
myRev xs    = xs


tqc2a = quickCheck (prop_tqc2 myRev)

tqc2b = quickCheck (prop_tqc2 reverse)

{-
 
    Which of the following statements hold?

    A: tqc2a succeeds (OK) but tqc2b fails (Failed).

    B: tqc2b succeeds (OK) but tqc2a fails (Failed).

    C: tqc2a and tqc2b both succeed (OK).

-}

-- Frage 4

-- tqc3

strangeRev :: String -> String
strangeRev xs
  | length xs < 100 = reverse xs
  | otherwise       = []


prop_tqc3 :: String -> Bool
prop_tqc3 xs = strangeRev (strangeRev xs) == xs

tqc3 = quickCheck prop_tqc3


quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }


tqc3N n = (quickCheckN n) prop_tqc3

{-

    Why is it possble that evaluation of tqc3 succeeds (OK).

    A: There must be a bug in quickCheck.

    B: quickCheck generates a fixed number of test data (by default 100).
       Among the generated test data there might not be an input of lenght
       greater than 100 and therefore the buggy case in strangeRev
       will not be executed.

    C: The property is wrong.

-}

-- Frage 5

-- tqc4

-- Consider

mySum [] = 1
mySum (x:xs) = x + mySum xs

prop_tqc4 :: [Int] -> Bool
prop_tqc4 xs = mySum xs /= sum xs

tqc4 = quickCheck prop_tqc4

{-

  Which of the following statements hold?

  A: prop_tqc4 yields False for any input.

  B: prop_tqc4 yields True for any input. --> Auf /= Achten!!!!! Deshalb

  C: There are inputs for which prop_tqc4 yields False.

-}