import Data.Char

get3Char:: IO ()
get3Char=do putStr "Enter three characters:"
            c1<-getChar
            c2<-getChar
            c3<-getChar
            putStr (c1:c2:c3:[])
getNChar::Int->IO String
getNChar 0 = return ""
getNChar n = do putStr ("Enter " ++ show n ++ " characters\n")
                ch<-getChar
                chs<- getNChar (n-1)
                return (ch:chs)
readLine::IO String
readLine = do ch <- getChar
              if ch == '\n' then
                return ""
              else do chs <- readLine
                      return (ch:chs)
readCharsPredicate::(Char->Bool)->IO String
readCharsPredicate p = do ch <- getChar
                          if (p ch) then
                            do chs <- (readCharsPredicate p)
                               return (ch:chs)
                          else return ""
char2Digit::Char->Int
char2Digit d=ord d - ord '0' 

readNum::IO Int
readNum=do digits <- readCharsPredicate isDigit
           return (foldl (\acc x -> (10 * acc) + (char2Digit x)) 0 digits)

getNumSigned::IO Int
getNumSigned = do x<-getChar
                  if (x== '-') then 
                    do n <- (getNum' 0)
                       return (-n)
                  else do n <- getNum' (char2Digit x)
                          return n
getNum' m = do xs <- readCharsPredicate isDigit
               return (foldl (\acc x ->  (acc*10)+ (char2Digit x)) m xs)  

getKeyvalue::IO (String, Int)
getKeyvalue = do key <- readCharsPredicate (\x-> x /= '=')
                 value <- readNum
                 return (key, value)

data Student = Student String Int
instance Show Student where
    show (Student name age)= "Student(" ++ name ++ "," ++show age ++ ") " 

getStudent::IO Student
getStudent = do name<-getLine
                age<-readNum
                return (Student name age)
