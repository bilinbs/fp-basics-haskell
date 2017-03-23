--Input : n (number to be checked for primality), b (base which is probably a witness)
mrPrime::Integer->Integer->Primality
mrPrime n b = let 
                (r,d) = factorOut2 (n-1)
                b0 = modPow b d n
              in 
                if (b0 == 1 || b0 == n-1)  then
                    ProbablyPrime
                else
                    mrPrime' n r b0
mrPrime'::Integer->Integer->Integer->Primality
mrPrime' n r bi | r==0 = if (bi == n-1) then
                            ProbablyPrime
                         else
                            Composite
                | otherwise = let 
                                bi2 = modPow bi 2 n
                              in
                                if (bi2 == n-1) then
                                    ProbablyPrime
                                else
                                    mrPrime' n (r-1) bi2

--factors out powers of 2 from a given number
--input n , output (r,d) such that n=2^r + d
factorOut2::Integer->(Integer,Integer)
factorOut2 n = factorOut2' n 0
factorOut2'::Integer->Integer->(Integer,Integer)
factorOut2' d r | (mod n 2) == 0 = (factorOut2' (quot d 2) (r+1)) 
              | otherwise = (r,d)

--Modular exponentiation (x^y mod m)
--modPow x y m
modPow::Integer->Integer->Integer->Integer
modPow x y m = mod (x ^ y) m

data Primality = Composite | ProbablyPrime
                    deriving Show 



