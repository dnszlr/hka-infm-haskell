-------------------------------------------------------
-- Extended Haskell programming exercise
-- Topic: functions over lists
-- Author: Dennis Zeller
-------------------------------------------------------



-- A student is represented by her name, student id and
-- a list of the courses the student is taking


type Student = (String, Int, [Int])
type DB = [Student]

-- TASK 0
{-
Databases must be consistent.
We say the database is consistent if there're no multiple entries
of students, and no multiple entries of courses per students
 For example, the below databases are *inconsistent*
-}

incons1 :: [([Char], Int, [Int])]
incons1 = [("Jack", 111, [141, 252, 141])]
incons2 :: [([Char], Int, [Int])]
incons2 = [("Jane", 112, [141, 252]), ("Jane", 112, [141, 252])]

{-
Your task is to implement the following function
which returns True if the database is valid (consistent),
otherwise the function returns False.
-}
valid :: DB -> Bool
valid db = validA db && validB db && validC db


validA [] = True
validA (x:xs)
    | x `elem` xs = False
    | otherwise = validA xs



validB [] = True
validB xs = and (map (\(_,_, courses) -> noDubs courses) xs)

noDubs [] = True
noDubs (x:xs)
    | x `elem` xs = False
    | otherwise = noDubs xs


-- EXTENSION TO TASK 0
{-
Extension: We strengthen the notion of consistency.
In addition, we require that there shall by no duplicate student id's.
For example,
-}

incons3 = [("Jane", 111, [141]), ("Jack", 111, [141, 252])]

getIds = map (\(_,id,_) -> id)

validC [] = True
validC xs = noDubs (getIds xs)


-- FROM HERE ON, WE WILL ASSUME THAT ALL DATABASES ARE CONSISTENT !!!
cons :: [([Char], Int, [Int])]
cons = [("Dennis", 111, [111, 112, 113]), ("Fynn", 112, [114, 115, 116]), ("Simon", 113, [117, 118, 119])]

-- TASK 1
{-
Given a database and a student id, we're looking for the list of
courses of this particular student.
-}

getCourses (_,_,courses) = courses
getId (_,id,_) = id
query1 :: DB -> Int -> [Int]
query1 [] id = []
query1 db id
    | getId (head db) == id = getCourses (head db)
    | otherwise = query1 db id



-- TASK 2
{-
Given a database and a course, find all students
taking this course.
-}
query2 :: DB -> Int -> [String]
query2 [] courseId = []
--query2 db courseId = getNames (getCourse courseId db)
query2 db courseId = map (\(name,_,_) -> name) (filter (\(_,_,courses) -> courseId `elem` courses) db)


getNames = map (\(name,_,_) -> name)

getCourse courseId = filter (\(_,_,courses) -> courseId `elem` courses)

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