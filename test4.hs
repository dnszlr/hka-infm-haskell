-- Frage 1

-- Consider

instance Num a => Num (Maybe a) where
    (+) (Just x) (Just y) = Just (x + y)
    (+) _ _               = Nothing

-- for brevity we omit definitions for (*) etc.

n1 = (Just 1) + (Just 3)


{-

    What is the result of evaluating n1?

    A: 4
    B: Just 4
    C: Nothing

-}

-- Frage 2

-- Consider

data E = One | Zero | ETrue | EFalse
       | Plus E E | Mult E E
       | Or E E  deriving Show


ex1 = Mult Zero (Plus Zero One)

{-

   We automatically derive an instance of Show for E.
   What is the result of evaluating "show ex1"?

    A:  Mult Zero Plus Zero One
    B:  (Mult Zero (Plus Zero One))
    C:  Mult Zero (Plus Zero One) 
-}

-- Frage 3

data List a = Nil | Cons a (List a)

member x Nil         = False
member x (Cons y ys)
  | x == y           = True
  | otherwise        = member x ys


{-

   What is the type of member?

   A:  Eq a => a -> List a -> Bool
   B:  a -> List a -> Bool
   C:  (a -> a -> Bool) -> a -> List a -> Bool

-}


-- Frage 4

-- Consider


data Square = Square Int deriving Show


class Show a => GeoShape a where
  area :: a -> Int
  scale :: a -> Int -> a

instance GeoShape Square where
  area (Square x)    = x * x
  scale (Square x) s = Square (x * s)


{-
 
     On its own, the above Haskell code yields an error. Why?

     A: There's no main routine.
     B: An instance of Square for Show is missing.
     C: Defining one geometric object is not enough.

-}
