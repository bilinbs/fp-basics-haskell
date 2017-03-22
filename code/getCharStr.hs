get2CharStr::IO String
get2CharStr=getChar >>= (\ch1 -> getChar >>= (\ch2-> return [ch1,ch2]))

readPrint3Chars::IO ()
readPrint3Chars = putStr "Enter 3 characters : " >> getnCharStr 3 >>= (\chars -> putStrLn ("\nThe three characters entered are " ++ chars))
getnCharStr::Int->IO String
getnCharStr n = getnCharStr' n ""
getnCharStr'::Int->String->IO String
getnCharStr' 0 acc = return acc
getnCharStr' n acc = getChar >>= (\ch -> getnCharStr' (n-1) (acc ++ [ch])) 


