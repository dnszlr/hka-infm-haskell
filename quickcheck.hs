import System.Random
import Data.Char
import Test.QuickCheck

-- To install System.Random and Test.QuickCheck
-- cabal install random
-- cabal install --lib random
-- cabal install QuickCheck
-- cabal isntall cabal install --lib QuickCheck
-- 
-- Notes for quickchecking in haskell 06.05.22-- | Counting words.
-- Word = Sequence of characters not separated by white spaces (blanks).
count :: Num a => String -> a
count [] = 0
count (c:cs)
  | c == ' ' = count $ skipBlanks cs
  | otherwise = 1 + count (skipWord cs)

-- | Generic skip function.
skip :: (Char -> Bool) -> String -> String
skip p [] = []
skip p (c:cs)
 | p c       = skip p cs
 | otherwise = c:cs

skipWord   = skip (/= ' ')
skipBlanks = skip (== ' ')


-- generator = Erzeugung zufälliger Testdaten
-- arbitrary = Über Type classes können Testdaten für alle möglichen Datentypen erzeugt werden.

-- QuickcheckLight

class Arbitrary a where
   arbitrary :: IO a 

-- | Choose one of the elements.
elements :: [a] -> IO a
elements xs = do i <- randomIO :: IO Int
                 return (xs !! (i `mod` (length xs)))
{-
-- Comes with quickCheck
-- | Generate a fixed number of arbitrary values.
vector ::  Arbitrary a => Int -> IO [a]
vector 0 = return []
vector n
    | n > 0  = do x <- arbitrary
                  xs <- vector (n-1)
                  return (x:xs)
    | otherwise = error "impossible"

instance Arbitrary Char where 
   arbitrary = do x <- elements [0..255]
                  return (chr x)

instance Arbitrary a => Arbitrary [a] where
   arbitrary = vector 2
instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
   arbitrary = do b <- randomIO 
                  if b 
                    then do l <- arbitrary 
                            return $ Left l
                    else do r <- arbitrary 
                            return $ Right r

genStrings :: IO ()
genStrings = do xs <- arbitrary
                putStrLn xs

-}
-- | Number of words counted must be greater or equal zero.
prop1 :: String -> Bool
prop1 s = count s >= 0

-- | Reversing the string yields the same number of words.
prop2 :: String -> Bool
prop2 s = count s == count (reverse s)

{-
quickCheck :: (Show t, Arbitrary t) => (t -> Bool) -> IO ()
quickCheck prop = go 100
   where go 0 = putStrLn "+++ Ok"
         go n = do x <- arbitrary
                   if prop x
                     then go (n-1)
                     else do putStrLn "*** Failed: "
                             print x

-}
-- | Concatenating the string doubles the number of words.
prop3 :: String -> Bool
prop3 s = 2 * count s == count (s ++ s)     


yolo = "Dennis Zeller"

-- = 4 weil: 2 * count (Dennis Zeller) = 4
counter1 = 2 * count yolo
-- = 3 weil: counter(Dennis ZellerDennis Zeller) = 3. Ist nur gleich wenn sich am Anfang ein Leerzeichen befindet.
counter2 = count (yolo ++ yolo)

