data EllpCurv = EllpCurv Int Int 

addEllP::EllpCurv->(Int,Int)->(Int,Int)->Maybe (Int,Int)
addEllP (EllpCurv a b) (x1,y1) (x2,y2) = if ((pointInCurve (x1,y1)) && (pointInCurve (x2,y2)))
                                            Just (x3,y3) 
                                            where
                                                m = (y2-y1) / (x2-x1)
                                                x3 = m*m - x1 -m2