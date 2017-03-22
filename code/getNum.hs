import Data.Char

getNum::IO Int
getNum = do chs <- getCharsPredicate isDigit
            return (parseIntFromString chs)

getCharsPredicate::(Char->Bool)->IO String
getCharsPredicate p = do ch <- getChar
                         if (p ch)
                         then do chs <- getCharsPredicate p
                                 return (ch:chs) 
                         else return ""

char2Digit::Char->Int
char2Digit ch = (ord ch) - (ord '0')


{-parseIntFromString::String->Int
parseIntFromString xs = foldl (\acc x -> (char2Digit x) + (10 * acc)) 0 xs-}

{-parseIntFromString::String->Int
parseIntFromString xs = parseIntFromString' xs 0
parseIntFromString'::String->Int->Int
parseIntFromString' [] acc = acc
parseIntFromString' (x:xs) acc =parseIntFromString' xs ((acc * 10) + (char2Digit x)) -}

parseIntFromString::String->Int
parseIntFromString [] = 0
parseIntFromString (x:xs) = ((10 ^ (length xs)) * char2Digit x) + (parseIntFromString xs) 