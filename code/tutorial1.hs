import Data.Char
import Data.List




-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = map (\x-> if (mod x 2) == 0 then (x/2) else x) xs

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | mod x 2 == 0 = (x/2):(halveEvensRec xs)
                     | otherwise = x:(halveEvensRec xs) 

-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = filter (\x-> x > lo && x < hi) xs

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec [] = []
inRangeRec (x:xs) | x > lo && x < hi = x:(inRangeRec xs)
                  | otherwise = inRangeRec xs 


-- 3. countPositives: count the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = foldr (\x count -> if x > 0 then (count  + 1) else count) 0 xs

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec xs = countPositivesRec' 0 xs
countPositivesRec' n [] = n
countPositivesRec' n (x:xs) | x > 0 = countPositivesRec' (n+1) xs
                            | otherwise = countPositivesRec n xs 

-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x = x * 90 / 100;

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = foldr (\x total -> if (discount x) > 199 then total + (discount x) else total) 0 xs

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec xs= pennypincherRec' 0 xs
pennypincherRec' total [] = total
pennypincherRec' total (x:xs) = let disc = discount x
                                    in if disc < 199 then
                                            pennypincherRec' (total + disc) xs
                                        else 
                                            pennypincherRec' total xs

-- 5. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs= foldr (\x acc -> if (isDigit x) then acc * (digitToInt x) else acc) 1 xs

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec xs = multDigitsRec' 1 xs
multDigitsRec' n [] = n
multDigitsRec' n (x:xs) = if (isDigit x) then
                            multDigitsRec' (acc * digitToInt x) xs
                          else
                            multDigitsRec' acc xs 



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise "" = ""  
capitalise (x:xs) = (toUpper x) : map toLower xs 

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec "" = ""
capitaliseRec (x:xs) = (toUpper x):(strToLower xs)
strToLower "" = ""
strToLower (x:xs) = (toLower x):(strToLower xs)


-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title [] = []
title (x:xs) = (capitalise x):(map (\x->if (length xs) > 4 then capitalise x else x) xs) 

-- Recursive version
titleRec :: [String] -> [String]
titleRec [] = []
titleRec (x:xs) = (capitalise x):(capitaliseWordsBiggerN 4 xs)
capitaliseWordsBiggerN n [] = []
capitaliseWordsBiggerN n (x:xs) | length x > n = capitalise x : capitaliseWordsBiggerN n xs
                                | otherwise  = x : capitaliseWordsBiggerN n xs




-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind = undefined

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec = undefined

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind = undefined 



-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search = undefined

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec = undefined

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search = undefined


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains = undefined

-- Recursive version
containsRec :: String -> String -> Bool
containsRec = undefined

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains = undefined

