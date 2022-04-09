-- Kombination von Map Fold und Filter reichen für die meisten Operationen

-- A student is represented by her name, student id and
-- a list of the courses the student is taking
type Student = (String, Int, [Int])
type DB = [Student]

cons1 :: DB
cons1 = [("Dennis", 111, [123, 456])]
cons2 :: DB
cons2 = [("Dennis", 111, [123, 456]), ("Gianni", 112, [234, 567])]
cons3 :: DB
cons3 = [("Dennis", 111, [123, 456]), ("Gianni", 112, [234, 567]), ("Kris", 113, [888, 999])]

-- TASK 0
{-
Databases must be consistent.
We say the database is consistent if there're no multiple entries
of students, and no multiple entries of courses per students
 For example, the below databases are *inconsistent*
-}
incons1 :: DB
incons1 = [("Jack", 111, [141, 252, 141])]
incons2 :: DB
incons2 = [("Jane", 112, [141, 252]), ("Jane", 112, [141, 252])]

{-
Your task is to implement the following function
which returns True if the database is valid (consistent),
otherwise the function returns False.
-}
valid :: DB -> Bool
valid db = rule1 db && rule2 db && rule3 db

-- Eine Datenbank darf nicht zweimal den selben Studenten enthalten.
rule1 :: DB -> Bool
rule1 db = not (contains db)

-- Eine Datenbank darf keinen Studenten haben, der zweimal im selben Kurs ist.
rule2 :: DB -> Bool
-- rule2 db = or (map (\(_,_,course) -> not (contains course)) db)
rule2 db = contains list
    where list = map(\(_,_,course) -> course) db

contains [] = False
contains [x] = False
contains (x:xs) = if elem x xs
                  then True
                  else contains xs

-- EXTENSION TO TASK 0
{-
Extension: We strengthen the notion of consistency.
In addition, we require that there shall by no duplicate student id's.
For example,
-}
incons3 :: DB
incons3 = [("Jane", 111, [141]), ("Jack", 111, [141, 252])]

rule3 :: DB ->  Bool
rule3 db = let list = map(\(_, id, _) -> id) db
           in list == filterDub list

filterDub [] = []
filterDub (x:xs) = if elem x xs
                   then filterDub xs
                   else x : filterDub xs

-- FROM HERE ON, WE WILL ASSUME THAT ALL DATABASES ARE CONSISTENT !!!

-- TASK 1
{-
Given a database and a student id, we're looking for the list of
courses of this particular student.
-}
-- Filter needed
query1 :: DB -> Int -> [Int]
query1 db id = flatten (map (\(_,_,cs) -> cs) (filter (\(_,i,_) -> i == id) db))

flatten :: [[a]] -> [a]
flatten xss = [ x | xs <- xss, x <- xs ]

printer db id = map (\(_,_,cs) -> cs) (filter (\(_,i,_) -> i == id) db)

-- TASK 2
{-
Given a database and a course, find all students
taking this course.
-}
query2 :: DB -> Int -> [String]
query2 = error "Your code"

-- TASK 3
{-
Given a database, sort the database (in non-decreasing order)
according to the name of students.
-}
sortDB :: DB -> DB
sortDB = error "Your code"

{-
Extension1:
Provide a function sortDB' which sorts the database according to the number of courses a student is taking

Extension2:
Provide a function sortDB'' which is parameterized in the actual comparing relation which determines the sorting order
For example:
 Given

cmpName :: Student -> Student -> Ordering
cmpName (n1, _, _) (n2, _, _) =
 if n1 < n2
 then LT
 else if n1 == n2
      then GT
      else EQ

Then you can define

 sortDB = sortDB'' cmpName

-}


-- TASK 4
{-
Given two databases, merge them to obtain one (consistent) database
 Example:

 merge [("Jane", 112, [141, 353])] [("Jane", 112, [141, 252])]
    => [("Jane", 112, [141, 252, 353])]

-}

merge :: DB -> DB -> DB
merge = error "Your code"