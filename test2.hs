--Frage1

--skip [] = []
--skip (x:y:ys) = x : skip ys

{-
   What is the result of evaluating skip "abcd"?

   A: "abcd"
   B: "cd"
   C: "ac"

skip "abcd"
a skip cd
a c skip d
a c


-}

-- Frage 2

-- last2 (x:xs) = last2 xs
-- last2 [x] = x

{-
     What is the problem (if any) with the above definition?

     A: There's no problem.
     B: Patterns (x:xs) and [x] overlap. As we first try (x:xs)
        a call such as last2 [1] reduces to last2 [] for which no match exists.
        We need to switch the order among the two function equations.
-}

-- Frage 3

hl2 = 'h' : "ello"

{-

    What is the result of evaluating hl2?

    A: "hello"
    B: Yields type error
    C: "ello"

-}

-- Frage 4

hl1 = [ 'a', 'b', 'c']

{-

   Which of the following options are equivilant to hl1?

    A: "'a''b''c'"
    B: "abc"
    C: "abc\0"

-}

-- Frage 5

mm [] = []
mm (True:xs) = False : mm xs
mm (False:xs) = True : mm xs


{-

     Which of the following is not an NOT equivalent definition of function mm?

     A: mmA xs = [not x | x <- xs ]
     B: mmB = map not --> 
     C: mmC xs = map (\x -> x) xs

-}

-- Frage 6

--gg [] = []
--gg (True:xs) = gg xs
--gg (False:xs) = True : gg xs

--gga = (map not) . (filter not)

{-

     Which of the following is equivalent to function gg?

     A: (map not) . (filter not) --> Würde drehen also nein
     B: (filter not) . (map not) --> ?! wtf
     C: map . (filter not) --> Richtig

-}

-- Frage 7

--last3 [x] = x
--last3 (x:xs) = last3 xs

--len [] = 0
--len (x:xs) = 1 + len xs

--last3b xs
--             | len xs == 1  = head xs
--             | len xs > 1   = last3 (tail xs)


{-

     Provide for an alternative definition of tail2 that only makes use of len, head and tail.

     A: This is not possible.
     B:  
          last3 xs
             | len xs == 1  = head xs
             | len xs > 1   = last3 (tail xs)

-- [1,2,3]
last3 [1,2,3]
len [1,2,3] = 3
last3 [2,3]
len [2,3] = 2
last3 [3]
len [3] = 1
last3 = 3


-}


-- Frage 8

--ff [] = []
--ff (True:xs) = ff xs
--ff (False:xs) = False : ff xs

{-

     Which of the following is not an NOT equivalent definition of function ff?

     A: ffA xs = [x | x <- xs, not x ]
     B: ffB xs = filter (\x -> x) xs --> Die isses net
     C: ffC = filter not

-}