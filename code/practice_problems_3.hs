import Data.Char
--1. Write a function which accepts a list of integer as input and returns sum of largest and second largest elements. What is the complexity of your program? The numbers of the list can be assumed to be all distinct.

sumOfLargestTwo::[Int]->Int
sumOfLargestTwo [] = 0
sumOfLargestTwo xs = l + sl 
                        where 
                            (l,sl) = getLargestTwo xs
getLargestTwo::[Int]->(Int, Int)
getLargestTwo (x:[]) = (x, 0)
getLargestTwo (x1:x2:xs) | x1 > x2 = getLargestTwo' x1 x2 xs
                       | otherwise = getLargestTwo' x2 x1 xs
getLargestTwo'::Int->Int->[Int]->(Int, Int)
getLargestTwo' l sl [] = (l, sl)
getLargestTwo' l sl (x:xs) = if x > l then
                                getLargestTwo' x l xs
                             else if x > sl then
                                    getLargestTwo' l x xs
                                  else 
                                    getLargestTwo' l sl xs
--2. Write a funtion which accepts two numbers a and b and return the list of Fibonacci numbers between a and b, in order. Explain the complexity of your program.

fib::Int->Int->[Int]
fib a b = a:(fib b (a+b))

fibList::[Int]
fibList = fib 0 1

fibBetween::Int->Int->[Int]
fibBetween a b = fibBetween' a b fibList

fibBetween'::Int->Int->[Int]->[Int]
fibBetween' a b (x:xs) | x < a = (fibBetween' a b xs)
                       | (x>a && x<b) = x:(fibBetween' a b xs)
                       | otherwise = []

--3. Using lambda calculus, evaluate (1+1)*1
-- Successor Function, S=\wyx.y(wyx)
-- Sum of Two numbers a and b, a + b = a S b
-- Multiplication, M=\xyz.x(yz)
-- Product of two numbers a,b a*b = M a b
-- (1 + 1) * 1 = (1S1)M1
-- In lambda calculus, 1 = \sz.s(z)
-- (1+1)*1  = (1S1)M1
--          = (\sz.s(z) \wyx.y(wyx) 1) M 1          {...substituting value of first 1 and S}
--          = ([\wyx.y(wyx) / s, 1/z] s(z)) M 1     {...applying function parameters}
--          = (\wyx.y(wyx) 1) M 1                   {...reducing}
--          = ([1 / w] \yx.y(wyx)) M 1              {...applying function parameters}
--          = (\yx.y(1yx)) M 1                      {...reducing}
--          = (\yx.y(\sz.s(z) y x)) M 1             {...substituting value of 1}
--          = (\yx.y([y / s, x / z] s(z))) M 1      {...applying function parameters}
--          = (\yx.y(y(x))) M 1                     {...reducing}
--          = ([M / y, 1 / x] y(y(x)))              {...applying function parameters}
--          = (M(M(1)))                             {...reducing}
--          = M (\xyz.x(yz) (1))                    {...substituting value of inner M}
--          = M ([1 / x] \yz.x(yz))                 {...applying function parameters}
--          = M (\yz.1(yz))                         {...reducing}
--          = M (\yz. (\sz.s(z)) (y z))             {...substituting value of 1}
--          = M (\yz. (\su.s(u)) (y z))             {...using alpha equivalence}
--          = M (\yz. ([y / s, z / u] s(u)))        {...applying function parameters}
--          = M (\yz. y(z))                         {...reducing}
--          = \xyz.x(yz) (\ab.a(b))                 {...substituting value of M}
--          = [\ab.a(b) / x] \yz.x(yz)              {...applying function parameters}
--          = \yz. \ab.a(b) (yz)                    {...reducing}
--          = \yz. [y /a, z /b] a(b)                {...applying function parameters}
--          = \yz. y(z)                             {...reduing}
--          = \sz. s(z)                             {...using alpha equivalence}
--          = 1
--4. 
--  a. Define a function to square numbers in  lambda calculus.
--  Multiplication function, M=\xyz.x(yz)
--  Square function for , SQ = \a.(M a a)
--      SQ  = \a.(\xyz.x(yz) a a)
--          = \a.(\z.a(az))
--          = \az.a(az)
--          = \xy.x(xy)

--  b. Evaluate 1^2 using your square function
--  In lambda calculus, 1 = \sz.s(z)
--  Square of one,
--    S 1   = \xy.x(xy) 1                       {...substituting value of 1}
--          = \y.1(1 y)                         {...applying function parameters and reducing}
--          = \y.1 (\sz.s(z) y)                 {...substituting value of inner 1}
--          = \y.1 (\z.y(z))                    {...applying function parameters and reducing}
--          = \y. \sx.s(x) (\z.y(z))            {...substituting value of 1}
--          = \y. \x. (\z.y(z) x)               {...applying function parameters and reducing}
--          = \y. \x. (y(x))                    {...applying function parameters and reducing}
--          = \yx.y(x)                          {...reducing to the convenience notation}
--          = \sz.s(z)                          {...using alpha equivalence}
--          = 1
--5. Infer the type of the variable f in each of the following definitions
--  a. 
--      g::[Int]
--      g = foldr f [] [1,2,3]

--   ANS:   f::Int->[Int]->[Int]
--  b.
--      g::[String]
--      g = foldl f [] [1,2,3]

--   ANS:   f::[String]->Int->[String]
--6. Define a function msearch which accpets an infinite list of monotonously increasing integers (if m comes before n in the list, m<=n), and another integer k and returns True if k is in the list. The function should return False otherwise. For example, 8 is in he list [2..], but not in the list [10..]
msearch::Int->[Int]->Bool
msearch _ [] = False
msearch a (x:xs) | a > x = msearch a xs
                 | (a == x )= True
                 | otherwise = False  
--7. 
--  a. Define a Tree datatype in which only leaves carry labels.
data Tree a = Leaf a | Node (Tree a) (Tree a) | Null
                deriving Show

--  b. Define a filterTree  function which takes a predicate and a tree and drops all the leaves whose labels do not satisfy the predicate.
filterTree::Tree a->(a->Bool)->Tree a
filterTree Null _ = Null
filterTree (Leaf x) p | p x = Leaf x
                      | otherwise = Null
filterTree (Node x y) p = Node (filterTree x p) (filterTree y p)  

--8. A custom list datatype is defined here.
data List a = NullList | Cons a (List a)
--  Write necessary declarations for making this List a member of the Eq typeclass. Two lists should be considered equal if and only if both the lists contain the same elements, but in any order. Assume that elements in a list are all distinct.
instance (Eq a, Ord a) => Eq (List a) where
    (==) NullList NullList = True
    (==) NullList _ = False
    (==) _ NullList = False
    (==) xs ys = if (lengthList xs) == (lengthList ys) then
                    let (Cons x xs1) = sort xs 
                        (Cons y ys1) = sort ys 
                    in (x==y) && (xs1==ys1)
                 else False
    (/=) NullList NullList = False
    (/=) _ NullList = True
    (/=) NullList _ = True
    (/=) (Cons x xs) (Cons y ys) = (x/=y) || (xs/=ys)

lengthList::List a->Int
lengthList NullList = 0
lengthList (Cons x xs) = 1 + (lengthList xs)

sort::Ord a=>List a -> List a
sort NullList = NullList
sort (Cons x NullList) = (Cons x NullList)
sort (Cons x xs) = insert x (sort xs)

insert::Ord a=>a->List a->List a
insert x NullList = Cons x NullList
insert x (Cons y ys) = if x <= y then 
                        Cons x (Cons y ys)
                       else
                        Cons y (insert x ys) 
--9. Write an IO function to input a positive floating point number. The number can have any number of integer digits and exactly two decimal digits. The integer and fractional parts are separated by a single decimal point symbol.

--getFloat::IO Float 
getFloat = do d <- getNumTillDot
              f <- getNDecimals 2
              return (d+f)

--getNumTillDot::IO Int
getNumTillDot = do num <- getCharsPredicate (\x -> x /= '.')
                   return (strToInt num)

getCharsPredicate::(Char->Bool)->IO String
getCharsPredicate p = do ch <- getChar
                         if p ch then
                            do chs <- getCharsPredicate p
                               return (ch:chs)
                         else return ""

--getNDecimals::Int->IO Float
getNDecimals 0 = return 0
getNDecimals n = do str <- getNDigits n
                    let a = (strToInt str)
                        b = (pow 10 n)
                        in return (a/b)

getNDigits::Int->IO String
getNDigits 0 = return ""
getNDigits n = do ch <- getChar
                  if (isDigit ch) then
                    do chs <- getNDigits (n-1)
                       return (ch:chs)
                  else 
                    do return ""

--strToInt::String->Int
strToInt xs = foldl (\acc x -> (acc*10) + (char2Digit x)) 0 xs

--char2Digit::Char->Int
char2Digit ch = ord ch - ord '0'

--pow::Int->Int->Int
pow _ 0 = 1
pow x n = x * (pow x (n-1))  


--10. The following datatype is in order to accommodate possible errors in programs. If there is an error, the programmer can retun an error string. Otherwise, the desired return value can be returned. Write the necessary definitions in order to declare 'ER a' to be a Monad.
--  data ER a = ERROR String | NOERROR a
--  The definition of the bind operator (>>=) should be such that in case the first argument produces and ERROR message, the value of the whole expression should be this error message.

data ER a = ERROR String | NOERROR a 

instance Monad ER  where
    --return::a->ER a
    return x = NOERROR x
    --(>>=):: ER a -> (a -> ER b) -> ER b
    f >>= g = case f of 
                ERROR x -> ERROR x
                NOERROR x -> g x
