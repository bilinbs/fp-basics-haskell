data Student = Student String Int Sem String
data Sem = I | II | III | IV  | V | VI | VII | VIII

instance Eq Student where
    (==) (Student name1 age1 sem1 stream1) (Student name2 age2 sem2 stream2) = (name1 == name2) && (age2==age1) && (sem1 == sem2) && (stream1 == stream2)

instance Eq Sem where
    (==) I I = True
    (==) II II = True
    (==) III III = True
    (==) IV IV = True
    (==) V V = True
    (==) VI VI = True
    (==) VII VII = True
    (==) VIII VIII = True
    (==) I _ = False
    (==) II _ = False
    (==) III _ = False
    (==) IV _ = False
    (==) V _ = False
    (==) VI _ = False
    (==) VII _ = False
    (==) I _ = False
    
getName::Student->String
getName (Student name _ _ _) = name

getAge::Student->Int
getAge (Student _ age _ _) = age

getSem::Student->Sem
getSem (Student _ _ sem _) = sem

getStream::Student->Sem
getStream (Student _ _ _ stream) = stream


data List a = Cons a (List a) | NullList
                deriving Show

instance Eq a => Eq (List a) where 
    (Cons x xs) == (Cons y ys) = (x==y) && (xs == ys)

reverseList::List a-> List a
reverseList NullList = NullList
reverseList (x `Cons` xs) = reverseList xs Main.++ (Cons x NullList)

(++)::List a -> List a -> List a
(Cons x NullList) ++ ys = Cons x ys
(Cons x xs) ++ ys = Cons x (xs Main.++ ys)

tripleEvens = \xs -> Main.map (\x-> 3*x) (Main.filter (\x-> (x `mod` 2) == 0) xs) 

map::(a->b)->List a -> List b
map f NullList = NullList
map f (x `Cons` xs) = (f x) `Cons` (Main.map f xs)

filter::(a->Bool)->List a -> List a
filter f NullList = NullList
filter f (Cons x xs) | (f x) = Cons x (Main.filter f xs)
                     | otherwise = Main.filter f xs

g xs = Main.filter (\x->x<20) (Main.filter(\x->x>5) (Main.map (\x->x*x) xs))

addLengthOfStr xs = foldr (\x acc -> (length x) + acc) 0 xs