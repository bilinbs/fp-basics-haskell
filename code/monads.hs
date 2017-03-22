import Data.Char

greatest::Ord a=> [a]-> Maybe a
greatest [] = Nothing
greatest (x:[]) = Just x
greatest (x1:x2:xs) | x1>x2 = greatest (x1:xs)
                    | otherwise = greatest (x2:xs)

{-greatestSum::[Int]->[Int]->[Int]->Maybe Int
greatestSum xs ys zs = case greatest xs of 
                            Nothing -> Nothing
                            Just gx -> case greatest ys of 
                                Nothing -> Nothing
                                Just gy -> case greatest zs of 
                                    Nothing -> Nothing
                                    Just gz -> Just (gx + gy + gz)
-}

{-greatestSum::[Int]->[Int]->[Int]->Maybe Int
greatestSum xs ys zs = greatest xs >>= 
                            \gx -> greatest ys >>= 
                                \gy -> greatest zs >>= 
                                    \gz -> return (gx + gy +gz)-}

greatestSum::[Int]->[Int]->[Int]->Maybe Int
greatestSum xs ys zs = do gx <- greatest xs
                          gy <- greatest ys
                          gz <- greatest zs
                          return (gx + gy + gz)

get3Chars::IO ()
get3Chars =  do putStr "Enter 3 characters: "
                ch1 <- getChar
                ch2 <- getChar
                ch3 <- getChar
                putStrLn ("\n The 3 characters entered are " ++ [ch1, ch2, ch3])

readPrintNChars::Int-> IO ()
readPrintNChars n = do putStr ("Enter " ++ (show n) ++ "characters: ")
                       chs <- getNChar n
                       putStrLn ("\n The entered " ++ (show n) ++ " characters are : " ++ chs)

getNChar::Int->IO String
getNChar 0 = return ""
getNChar n = do ch <- getChar
                chs <- getNChar (n-1)
                return (ch:chs)

readLine::IO String
readLine = do ch <- getChar
              if (ch == '\n') 
              then return ""
              else do  chs <- readLine
                       return (ch:chs) 

getCharsPredicate::(Char->Bool)->IO String
getCharsPredicate p = do ch <- getChar
                         if (p ch)
                         then do chs <- getCharsPredicate p
                                 return (ch:chs) 
                         else return ""



char2Digit::Char->Int
char2Digit ch = (ord ch) - (ord '0')

{-getNum::IO Int
getNum = do ch <- getChar
            if (isDigit ch)
            then do chs <- getNum 
                    return ((char2Digit ch) + (10 * chs))
            else return 0-}

getNum::IO Int
getNum = do chs <- getCharsPredicate isDigit
            return (parseIntFromString chs)

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