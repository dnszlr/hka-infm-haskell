-- Vorlesung vom 14.04.2022 & 21.04.2022 Data Types
-- Deriving == Vererben
data List a = Null | Cons a (List a) deriving (Show, Eq)
-- instance Show (List Int) ... was macht instance? Überladen?

