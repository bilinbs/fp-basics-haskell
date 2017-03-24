import Data.Maybe

addEllP::EllpCurve->(Integer,Integer)->(Integer,Integer)->Integer->Maybe (Integer,Integer)
addEllP (EllpCurve a b) (x1,y1) (x2,y2) p = if ((pointInCurve (EllpCurve a b) (x1,y1) p) && (pointInCurve (EllpCurve a b) (x2,y2) p)) then
                                            let
                                                m = if ((x1==x2) && (y1==y2)) then
                                                        modMult ((modMult 3 (modPow x1 2 p) p)+a) (inverse (modMult 2 y1 p) p) p
                                                    else
                                                        modMult (y2-y1) (inverse (x2-x1) p) p
                                                x3 = positiveConvMod ((modMult m m p) - x1 -x2) p
                                                y3 = positiveConvMod ((modMult m (positiveConvMod (x1-x3) p) p) - y1) p
                                            in
                                                Just (x3,y3)                                                    
                                           else Nothing

doubleEllp::EllpCurve->(Integer,Integer)->Integer->Maybe (Integer,Integer)
doubleEllp e (x1, y1) p = addEllP e (x1,y1) (x1,y1) p

nthMultEllp::EllpCurve->(Integer,Integer)->Integer->Integer->Maybe (Integer,Integer)
nthMultEllp (EllpCurve a b) (x1,y1) n p = let 
                                            (x2,y2) = fromJust (doubleEllp (EllpCurve a b) (x1,x2) p)
                                           in 
                                            nthMultEllp' (EllpCurve a b) (x2,y2) (x1,y1) (n-1) p
nthMultEllp' (EllpCurve a b) (accX,accY) (x1,y1) n p | n == 0 = Just (accX,accY)
                                                     | otherwise = let 
                                                                    (x2,y2) = fromJust (addEllP (EllpCurve a b) (accX,accY) (x1,y1) p)
                                                                   in
                                                                    nthMultEllp' (EllpCurve a b) (x2,y2) (x1,y1) (n-1) p
pointInCurve::EllpCurve->(Integer,Integer)->Integer->Bool
pointInCurve (EllpCurve a b) (x,y) p = (modPow y 2 p) ==positiveConvMod ((modPow x 3 p) + (modMult a x p) + (positiveConvMod b p)) p


modMult::Integer->Integer->Integer->Integer
modMult x y m = (x * y) `mod` m

--Modular exponentiation (x^y mod m)
--modPow x y m
modPow::Integer->Integer->Integer->Integer
modPow x y m = mod (x ^ y) m

--input: n m
--output n^-1 (mod m)
inverse::Integer->Integer->Integer
inverse n m = inverse' n (m-1) m

inverse' n x m | (modMult n x m) == 1 = x
               | otherwise = (inverse' n (x-1) m)

positiveConvMod::Integer->Integer->Integer
positiveConvMod n m | n<0 = positiveConvMod (n+m) m
                    | n>m = positiveConvMod (n-m) m
                    | otherwise = n

data EllpCurve = EllpCurve Integer Integer 