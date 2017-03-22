data BinTree a = SubTree a (BinTree a) (BinTree a)
                    | LeftSubTree a (BinTree a)
                    | RightSubTree a (BinTree a)
                    | Leaf a
                    deriving (Show)

doubleLabels::BinTree Int -> BinTree Int
doubleLabels (Leaf x) = Leaf (2 * x)
doubleLabels (LeftSubTree x xs) = LeftSubTree (2*x) (doubleLabels xs)
doubleLabels (RightSubTree x xs) = RightSubTree (2*x) (doubleLabels xs)
doubleLabels (SubTree x xs ys) = SubTree (2*x) (doubleLabels xs) (doubleLabels ys)

sumChildLabels::BinTree Int -> Int
sumChildLabels (Leaf x) = x
sumChildLabels (LeftSubTree x xs) = x+(sumChildLabels xs)
sumChildLabels (RightSubTree x xs) = x+(sumChildLabels xs)
sumChildLabels (SubTree x xs ys) = x+(sumChildLabels xs)+(sumChildLabels ys)

changeNodeLabelsToChildSums::BinTree Int -> BinTree Int
changeNodeLabelsToChildSums (Leaf x) = Leaf x
changeNodeLabelsToChildSums (LeftSubTree x xs) = LeftSubTree (x+ (sumChildLabels xs)) (changeNodeLabelsToChildSums xs)
changeNodeLabelsToChildSums (RightSubTree x xs) = RightSubTree (x+ (sumChildLabels xs)) (changeNodeLabelsToChildSums xs)
changeNodeLabelsToChildSums (SubTree x xs ys) = SubTree (x + (sumChildLabels xs) + (sumChildLabels ys)) (changeNodeLabelsToChildSums xs) (changeNodeLabelsToChildSums ys)