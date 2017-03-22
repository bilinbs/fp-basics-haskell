--Solutions for problems given in the file "fp model ques paper.pdf"

--Question 1
--*a = 1
--*b = 4
--*c = 4
--*d = 2
--*e = 1
--*f = 3
--*g = 4
--*h = 1
--*i = 3
--*j = 1
--*k = 2
--*l = 3

--Question 2
--* a
triangle::Int->Int
triangle 0 = 0
triangle n = n + triangle (n-1)

--* c
--init::[a]->[a]
--cons::a->[a]->[a]
--pair::a->b->(a,b)
--inc::Num a=>a->a
--twice::(a->a)->a->a

--* d
and1::[Bool]->Bool
and1 [] = True
and1 (x:xs) = x && (and1 xs) 

reverse1::[a]->[a]
reverse1 [] = []
reverse1 (x1:xs) = (reverse1 xs) ++ [x1]

replicate1::Int->a->[a]
replicate1 n x = replicate1' n x [] 
                    where
                        replicate1' 0 x xs = xs
                        replicate1' n x xs = replicate1' (n-1) x (x:xs)

filter1::(a->Bool)->[a]->[a]
filter1 f [] = []
filter1 f (x:xs) | (f x) == True = x:(filter1 f xs)
                | otherwise = filter1 f xs

--Question 3
--* a
insert::Int->[Int]->[Int]
insert x [] = [x]
insert x (y:ys) = if x <= y then 
                    x:y:ys
                  else
                    y:(insert x ys) 

--* c
isort::[Int]->[Int]
isort [] = []
isort (x:xs) = insert x (isort xs)

--* e
smaller::Int->[Int]->[Int]
smaller n xs = filter (\x -> x < n) xs

larger::Int->[Int]->[Int]
larger n xs = filter (\x -> x > n ) xs

--* f
qsort::[Int]->[Int]
qsort [] = []
qsort (x:xs) = qsort (smaller x xs) ++ [x] ++ qsort (larger x xs)

--Question 4
data Tree = Leaf Int | Node Tree Tree
            deriving Show

--* a
-- t1::Tree = Leaf 1
-- t2::Tree = Node (Leaf 1) (Leaf 2)
-- t3::Tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

--* b
leaves::Tree->[Int]
leaves (Leaf n) = [n]
leaves (Node xs ys) = (leaves xs) ++ (leaves ys)

size::Tree->Int
size xs = length (leaves xs)

--* c
balanced::Tree->Bool
balanced (Leaf _) = True
balanced (Node (Leaf _) (Node _ _)) = False
balanced (Node (Node _ _) (Leaf _)) = False 
balanced (Node xs ys) = (balanced xs) && (balanced ys)

--* d
halve::[a]->([a],[a])
halve [] = ([],[])
halve (x:xs) = halve' [x] xs
halve'::[a]->[a]->([a],[a])
halve' xs (y:ys) | (length xs >= length (y:ys)) = (xs, y:ys)
                 | otherwise = halve' (xs ++ [y]) ys 

--* e
balance::[Int]->Tree
balance [x] = Leaf x
balance xs = Node (balance first) (balance second)
                where (first, second) = halve xs