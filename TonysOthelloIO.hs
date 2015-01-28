data Strategy = St1 | St2 | St3 | DoesNotExist

instance Show (Strategy) where
  show s = strategy2Strn s

instance Eq (Strategy) where
  s1 == s2 = strategy2Strn s1 == strategy2Strn s2

strategy2Strn   :: Strategy -> String
strategy2Strn St1 = "st1"
strategy2Strn St2 = "st2"
strategy2Strn St3 = "st3"
strategy2Strn DoesNotExist = "This is not a strategy"

strn2Strategy   :: String -> Strategy
strn2Strategy "st1" = St1
strn2Strategy "st2" = St2
strn2Strategy "st3" = St3
strn2Strategy _ = DoesNotExist

putStrategy     :: Strategy -> IO()
putStrategy s = putStr (strategy2Strn s)

main = do
  putStrLn "Hello, Please enter the name of Strategy 1"

  let inputChecking a =
	  if (strn2Strategy a /= DoesNotExist)
	    then putStr "valid Strategy\n"
	  else
	    putStr "invalid Strategy\n"
  
  s1 <- getLine
  inputChecking s1

  putStrLn "Please enter the second Strategy"
  s2 <- getLine
  inputChecking s2
  return ()
 
