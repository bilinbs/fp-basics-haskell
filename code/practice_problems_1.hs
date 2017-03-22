--1. Write a function which takes in a sorted list and outputs the element which occur most often.
mostFreqElement::Eq a => [a]->a
mostFreqElement [x] = x
mostFreqElement (x:xs) = mostFreqElement' x 1 x 1 xs
mostFreqElement'::Eq a=>a->Int->a->Int->[a]->a
mostFreqElement' mostElt _ _ _ [] = mostElt
mostFreqElement' mostElt mostEltCnt currElt currEltCnt (x:xs) | (x == currElt) = if mostEltCnt < (currEltCnt + 1) then
                                                                                    mostFreqElement' x (currEltCnt + 1) x (currEltCnt + 1) xs
                                                                               else
                                                                                    mostFreqElement' mostElt mostEltCnt x (currEltCnt + 1) xs
                                                               | otherwise =  mostFreqElement' mostElt mostEltCnt x 1 xs
--2. Write an lambda calculus function for adding 1 to a number
--      \wyx.y(wyx)

--3. Write a function which receives two infinite lists as input and outputs a third inifinite list. Each element in the output list should be the sum of the corresponding elements in the input list

sumInfList::Num a=>[a]->[a]->[a]
sumInfList (x:xs) (y:ys) = (x+y):sumInfList xs ys
--4. (a) Define a Student datatype where each student has a name field and three marks fields.
data Student = Student String Int Int Int 
--   (b) Given a list of Students, write a function to return a list of the names of the Students with total marks (sum of the three marks) greater than 90.
studentNamesAbove90::[Student]->[String]
studentNamesAbove90 [] = []
studentNamesAbove90 xs = getStudNameList (topStudents xs 90)
getStudNameList::[Student]->[String]
getStudNameList [] = []
getStudNameList ((Student name _ _ _):xs) = name:getStudNameList xs
topStudents::[Student]->Int->[Student]
topStudents [] _ = []
topStudents ((Student name m1 m2 m3):xs) limit = let total = m1+m2+m3 in
                                                if total > limit then
                                                    (Student name m1 m2 m3):topStudents xs limit
                                                else 
                                                    topStudents xs limit

--5. Infer the types of f in the following expressions
--  (a) putStr (f ['a', 'b']) ::IO String
--  (b) putStr (f 23) ::IO Char
--  (c) getChar >>= f ::IO ()
        
--      f::Char->IO ()

--  (d) getStr >>= f >>= putStr ::IO ()

--      f::String->IO String  