data Tree a=Branch a (Tree a) (Tree a)
		|Leaf a
	deriving Show

sumofLabels::Tree Int->Tree Int
sumofLabels (Leaf x)=Leaf x
sumofLabels (Branch x xs ys)=Branch (sum1 (Branch x xs ys)) (sumofLabels xs) (sumofLabels ys)

sum1::Tree Int->Int
sum1 (Leaf x)=x
sum1 (Branch x xs ys)=x+(sum1 xs)+(sum1 ys)
