import Data.Maybe

--input List of (a,m) for a system of congruences x=a (mod m)
chineseRemainder::[(Integer,Integer)]->Maybe (Integer,Integer)
chineseRemainder [] = Nothing
chineseRemainder (x:[]) = Just x
chineseRemainder (x1:x2:xs) = let x = solveCongruence x1 x2
                              in 
                                if x ==Nothing then
                                    Nothing
                                else
                                    chineseRemainder ((fromJust x):xs)

--For system of congruences x=a (mod m) & x=b (mod n)
--solveCongruence::(a,m)->(b,n)->(x,mn)
solveCongruence::(Integer,Integer)->(Integer,Integer)->Maybe (Integer,Integer)
solveCongruence (a,m) (b,n) | (gcdiv m n) /= 1 = Nothing
                            | otherwise = let
                                            mInv = (inverse m n)
                                            k = (mod ((positiveConvMod (b-a) n) * mInv) n)
                                            mn =m*n
                                            x = (mod (a + (m * k)) mn)
                                          in
                                            Just (x,mn)



gcdiv::Integer->Integer->Integer
gcdiv m n | n ==0 = m
        | otherwise = gcdiv n (mod m n)


positiveConvMod::Integer->Integer->Integer
positiveConvMod n m | n<0 = positiveConvMod (n+m) m
                    | otherwise = n

modMult::Integer->Integer->Integer->Integer
modMult x y m = mod (x*y) m

--input: n m
--output n^-1 (mod m)
inverse::Integer->Integer->Integer
inverse n m = inverse' n (m-1) m

inverse' n x m | (modMult n x m) == 1 = x
               | otherwise = (inverse' n (x-1) m)
