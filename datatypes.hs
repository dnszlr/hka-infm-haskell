-- Haskell Part 2: Data Types und User-Controlled Overloading
data WeekDay = Mo | Tue | Wed | Thur | Fri | Sat | Sun

printWD :: WeekDay -> String
printWD Mo   = "Monday"
printWD Tue  = "Tuesday"
printWD Wed  = "Wednesday"
printWD Thur = "Thursday"
printWD Fri  = "Friday"
printWD Sat  = "Satursday"
printWD Sun  = "Sunday"

-- OK und Fail sind Konstruktoren
-- Ein Datentyp enthält also eine Anzahl an Konstrukturen die diesen definieren 
-- und mit diesen man anschließend weiterarbeiten kann
--                                 deriving Vererbung / Einbinden
data Result = Ok Int | Fail String deriving Show

failSafeDiv n 0 = Fail "Division by Zero"
failSafeDiv n m = Ok (n `div` m)

-- Neuer Datentyp
data DEither a b = DLeft a | DRight b
-- Typ Synonm? -> Was genau macht das? Nur zum aufrufen gedacht?
type TResult = DEither String Int

failSafeDiv2 :: Int -> Int -> TResult
failSafeDiv2 n 0 = DLeft "asd"
failSafeDiv2 n m = DRight (n `div` m)

-- Datentyp definieren
-- a und b können für DMaybe alles sein!
data DMaybe a b = TJust a | TNothing b deriving Show
-- Hier wird festgelegt, dass a = Int und b = String sein muss
type TMaybe = DMaybe Int String

failSafeDiv3 :: Int -> Int -> TMaybe
--                 Hier wird b mit dem String "Nicht korrekt!" übergeben
failSafeDiv3 n 0 = TNothing "Nicht korrekt!"
--                 Hier wird a mit dem Int (n `div` m) bzw. dem Ergebnis von n geteilt durch m übergeben
failSafeDiv3 n m = TJust (n `div` m)


data List a = Null | Cons a (List a) deriving Show

-- mfilter (>2) (Cons 1(Cons 2(Cons 3(Cons 4(Cons 5 Null)))))
-- Ergebnis = Cons 3 (Cons 4 (Cons 5 Null))
mfilter p Null = Null
mfilter p (Cons x xs)
   | p x       = Cons x (mfilter p xs)
   | otherwise = mfilter p xs


-- member 2 (Cons 1(Cons 2(Cons 3 Null)))
-- Ergebnis = True
-- Problem mit Beispiel der Wochentage von Zeile 2, da == nicht für diesen Datentyp definiert ist.
member x Null = False
member x (Cons y ys)
    | x == y    = True
    | otherwise = member x ys


-- Lösung 1. (monomorphic)
-- memberWD sun (Cons Mo(Cons Tue(Cons Thur Null)))
-- Ergebnis = False
memberWD x Null = False
memberWD x (Cons y ys)
    | eqWD x y    = True
    | otherwise = memberWD x ys

eqWD Mo Mo         = True
eqWD Tue Tue       = True
eqWD Wed Wed       = True
eqWD Thur Thur     = True
eqWD Fri Fri       = True
eqWD Sat Sat       = True
eqWD Sun Sun       = True
eqWD _ _           = False


-- Lösung 2. (higher-order functions)
-- Problem hierbei ist, dass jedes mal die eqWD Funktion mit in die Funktion übergeben werden muss
member2 eq x Null = False
member2 eq x (Cons y ys)
    | eq x y    = True
    | otherwise = member2 eq x ys

testMember2 = member2 eqWD Mo (Cons Tue (Cons Mo Null))

-- Lösung 3. (Instances => Way to go!)
instance Eq WeekDay where
-- Hier wird der (==) Operator für WeekDay so definiert, dass die Werte anhand von eqWD in Zeile 71 auf Gleichheit überprüft werden.
    (==) = eqWD

-- Funktioniert jetzt mit der ursprünglichen member Funktion, da (==) nun korrekt für Wochentage angewendet wird.
testMember3 = member Mo (Cons Mo(Cons Fri(Cons Sun Null)))