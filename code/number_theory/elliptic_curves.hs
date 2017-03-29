import Data.Maybe

--point addtion in elliptic curves
addEllP::EllpCurve->Point->Point->Integer->Point
addEllP curve point NullPoint _ = point
addEllP curve NullPoint point _ = point 
addEllP (EllpCurve a b) (Point x1 y1) (Point x2 y2) p 
                | x1 == x2 && y1 == (p - y2) = NullPoint
                | otherwise =   let
                                    m = if ((x1==x2) && (y1==y2)) then
                                            modMult ((modMult 3 (modPow x1 2 p) p)+a) 
                                                (inverse (modMult 2 y1 p) p) p
                                        else
                                            modMult (y2-y1) (inverse (x2-x1) p) p
                                    x3 = positiveConvMod ((modMult m m p) - x1 -x2) p
                                    y3 = positiveConvMod ((modMult m (positiveConvMod (x1-x3) p) p) - y1) p
                                in
                                    (Point x3 y3)                                                    
                  
--point doubling in elliptic curves
doubleEllp::EllpCurve->Point->Integer->Point
doubleEllp e point p = addEllP e point point p

--nth multiple ofa point in ellptic curves
nthMultEllp::EllpCurve->Point->Integer->Integer->Point
nthMultEllp curve point n p = let 
                                    p2 = (doubleEllp curve point p)
                                in 
                                    nthMultEllp' curve p2 point (n-1) p
nthMultEllp' curve acc p1 n p 
                | n == 1 = acc
                | otherwise = let 
                                p2 = 
                                    addEllP curve acc p1 p
                              in
                                nthMultEllp' curve p2 p1 (n-1) p


--input elliptic curve, two points p, q , integer field value m
--output n such that n.p = q
divEllp::EllpCurve->Point->Point->Integer->Integer
divEllp curve p q m = divEllp' curve p q m 2
divEllp' curve p q m n = let 
                            np = nthMultEllp curve p n m
                         in
                            if np == q then
                                n
                            else
                                divEllp' curve p q m (n+1) 

--checks whether a given point is in the curve
pointInCurve::EllpCurve->Point->Integer->Bool
pointInCurve (EllpCurve a b) (Point x y) p = 
    (modPow y 2 p) == positiveConvMod ((modPow x 3 p) + 
        (modMult a x p) + (positiveConvMod b p)) p

--Modular exponentiation (x^y mod m)
--modPow x y m
modPow::Integer->Integer->Integer->Integer
modPow x y m | y == 1          = mod x m
             | (mod y 2) == 0  = modPow (mod (x^2) m) (quot y 2) m
             | otherwise       = modMult x  (modPow x (y-1) m) m

--Modular multiplication (x*y mod m)
--modMult x y m
modMult::Integer->Integer->Integer->Integer
modMult x y m | y == 1         = mod x m
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
--convert negative numbers to positive in mod m world
positiveConvMod::Integer->Integer->Integer
positiveConvMod n m | n<0 = positiveConvMod (n+m) m
                    | n>=m = positiveConvMod (n-m) m
                    | otherwise = n

--data types
data EllpCurve = EllpCurve Integer Integer
instance Show EllpCurve where
    show (EllpCurve a b) = "y^2 = x^3 " ++ (signStr a) ++ 
        (show (abs a)) ++ "x "++ (signStr b) ++ (show (abs b)) 
data Point = Point Integer Integer | NullPoint
instance Eq Point where
    (==) (Point x1 y1) (Point x2 y2) = (x1 == x2) && (y1 == y2) 

instance Show Point where
    show (Point x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")" 
    show NullPoint = "O"

signStr::Integer->String
signStr x | x < 0 = "- "
          | otherwise = "+ "