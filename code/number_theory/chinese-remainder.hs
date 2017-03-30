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


--gratest commond divisor
--gcdiv m n 
gcdiv::Integer->Integer->Integer
gcdiv m n | n ==0     = m
          | otherwise = gcdiv n (mod m n)


--convert negative numbers to positive in mod m world
positiveConvMod::Integer->Integer->Integer
positiveConvMod n m | n<0 = positiveConvMod (n+m) m
                    | n>m = positiveConvMod (n-m) m
                    | otherwise = n

--Modular multiplication (x*y mod m)
--modMult x y m
modMult::Integer->Integer->Integer->Integer
modMult x y m | x < 0 || y < 0 = modMult (positiveConvMod x m) (positiveConvMod y m) m
              | y <= 1         = mod (x*y) m
              | (mod y 2) == 0 = modMult (mod (x*2) m) (quot y 2) m
              | otherwise      = mod (x + (modMult x (y-1) m)) m

--input: n m
--output n^-1 (mod m)
inverse::Integer->Integer->Integer
inverse a n = positiveConvMod (inverse' 0 n 1 a) n
inverse' t r nt nr | nr == 0 = t
                   | otherwise = let
                                    quotient = quot r nr
                                   in
                                    inverse' nt nr (t - quotient * nt) (r - quotient * nr)


