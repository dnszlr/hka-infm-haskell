-- Aufgabe 1
-- We wish to collect all elements that are stored in a tree.
-- We use a definition of a tree where all elements are stored in leaf nodes.

--data Tree a = Leaf a | Node (Tree a) (Tree a)
--collect :: Tree a -> [a]
--collect (Leaf x)   =  [x]
--collect (Node l r) = collect l ++ collect r



{-

collect :: Tree a -> [a]
collect (Leaf x)   =  [x]
collect (Node l r) = ???



  Complete the missing bits marked by ???

  A:   ??? replaced by collect l ++ [x] ++ collect r
  B:   ??? replaced by l ++ r
  C:   ??? replaced by collect l ++ collect r

-}

-- Aufgabe 2
-- There already exists a similar built-in data type, see Maybe.
data Option a = Null | Something a

-- Consider

div :: Float -> Float -> Float
div x y
 | y /= 0 = x / y

div2 :: Float -> Float -> Option Float
div2 x y
 | y /= 0 =    Something (x / y)
 | otherwise = Null

{-

The above is an example of a partial function.
We wish to provide for a total division function
where the exceptional case (division by zero) is
captured via the the above Option data type.

div2 :: Float -> Float -> ???
div2 x y
 | y /= 0 =    Something (x / y)
 | otherwise = !!!


   Complete the missing bits marked by ??? and !!!

  A:  ??? replaced by Option Float
      !!! replaced by Null
  B:  ??? replaced by Float
      !!! replaced by Null
  C:  ??? replaced by Null
      !!! replaced by Option Float

-}

-- Aufgabe 3

-- Consider

data E = One | Zero | ETrue | EFalse
       | Plus E E | Mult E E
       | Or E E deriving Show


eval :: E -> Maybe (Either Int Bool)
eval One = Just (Left 1)
eval Zero = Just (Left 0)
eval ETrue = Just (Right True)
eval EFalse = Just (Right False)
eval (Plus e1 e2) =
     let r1 = eval e1
         r2 = eval e2
     in case (r1, r2) of
         (Just (Left i1), Just (Left i2)) -> Just (Left (i1 + i2))
         (_,_)  -> Nothing
eval (Mult e1 e2) =
     let r1 = eval e1
         r2 = eval e2
     in case (r1, r2) of
         (Just (Left i1), Just (Left i2)) -> Just (Left (i1 * i2))
         (_,_)  -> Nothing
eval (Or e1 e2) =
     case (eval e1) of
        Nothing -> Nothing
        (Just (Right True)) -> Just (Right True)
        _                   -> case (eval e2) of
                                 Nothing -> Nothing
                                 (Just (Right True)) -> Just (Right True)
                                 _                   -> Just (Right False)

data Type = TInt | TBool deriving Show


test1  = (Or ETrue (Plus Zero ETrue))
test2 =  (Or Zero ETrue)
test3 = eval (Plus One (Or ETrue Zero))

{-

    Which of the following is an ill-typed expression whose evaluation via eval is successful?

    A: Or ETrue (Plus Zero ETrue)

    B: Or Zero ETrue

    C: Plus One (Or ETrue Zero)

-}

-- Frage 4
{-}
data D1 = K0 | K1

data D2 = K2 | K3 D1

data D3 = K4 | K5 D2


f :: D3 -> Int
f K4     = 0
f (K5 _) = 1


g :: D3 -> Int
g K4           = 0
g (K5 K2)      = 1
g (K5 (K3 K0)) = 1
g (K5 (K3 K1)) = 1

-}
{-

     Which of the following statements is true.

    A:   g and f are equivalent
    B:   g and f are not equivalent
    C:   g is a partial function

-}

-- Aufgabe 5
safeHead :: [a] -> Option a
safeHead []     = Null
safeHead (x:xs) = Something x

{-

    What is the type of function safeHead?

   A:  Option a
   B:  [a] -> Option a
   C:  [a] -> a

-}

-- Aufgabe 6
-- Consider the following contrived example.

data D1 = K0 | K1

data D2 = K2 | K3 D1

data D3 = K4 | K5 D2


f :: D3 -> Int
f K4     = 0
f (K5 K2) = 1
f (K5 (K3 K0)) = 1
f (K5 (K3 K1)) = 1

{-

   Instead of the don't care pattern "_" we introduce patterns to cover each case.
   How many pattern matching cases do we find then overall?

   A:  3
   B:  4
   C:  5

-}