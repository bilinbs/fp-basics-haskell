getNum::IO Int
getNum = getNextDigit 0

getNextDigit x = do ch <-  getChar 
                    if (isDigit ch)
                    then do num <- getNextDigit (10 * x + (char2Digit ch))
                            return num
                    else return x
