modPow::Integer->Integer->Integer->Integer
modPow x y m = mod (x ^ y) m

data Primality = Composite | ProbablyPrime

extract2::Integer->(Integer,Integer)
extract2 n = extract2' n 0
extract2' n r | (mod n 2) == 0 = (extract2' (floor (n/2)) (r+1)) 

mrPrime::Integer->Integer->Primality
mrPrime n k = let 
                (r,d) = extract2 (n-1)