import Data.Char

getCharsPred::(Char->Bool)->IO String
getCharsPred f= do ch<-getChar
                   if (f ch)
                   then do chs <- getCharsPred f 
                           return (ch:chs)
                   else return ""

char2Digit::Char->Int
char2Digit d = (ord d) - (ord '0')

parseInt::String -> Int
parseInt "" = 0
parseInt (x:xs) | x=='-' = (- parseInt' 0 xs)
                | otherwise = parseInt' 0 (x:xs)
parseInt'::Int->String->Int
parseInt' acc "" = acc
parseInt' acc (x:xs) = parseInt' ((acc * 10) + (char2Digit x)) xs

getNum::IO Int
getNum = do ch <- getChar
            if (isDigit ch || ch == '-')
            then do chs <- getCharsPred (isDigit)
                    return (parseInt (ch:chs))
            else return 0
        

getSum::IO Int
getSum = do n <- getNum
            if (n == 0)
            then return 0
            else  do sumn <- getSum
                     return (n + sumn) 