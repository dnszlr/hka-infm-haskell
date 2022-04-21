-- Haskell Part 2: Data Types und User-Controlled Overloading
data WeekDay = Mo | Tue | Wed | Thur | Fri | Sat | Sun

printWD :: WeekDay -> String
printWD Mo = "Monday"
printWD Tue = "Tuesday"
printWD Wed = "Wednesday"
printWD Thur = "Thursday"
printWD Fri = "Friday"
printWD Sat = "Satursday"
printWD Sun = "Sunday"

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

