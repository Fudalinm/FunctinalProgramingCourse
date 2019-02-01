actSeq = putChar 'L' >>= \_ -> putChar 'O' >>= \_ -> putChar 'L' >>= \_ ->  putChar '\n' >>= \_ -> putChar 'x'
actSeq' = putChar 'L' >> putChar 'O' >> putChar 'L' >>  putChar '\n' >> putChar 'x'


doActSeq = do
  putChar 'A'
  putChar 'G'
  putChar 'H'
  putChar '\n'
  
echo1 = getLine >>= putStrLn

doEcho1 = do
  line <- getLine
  putStrLn line
  
  
echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
  line <- getLine
  putStrLn $ line ++ "!"
  
  
echo3 :: IO ()
echo3 =  getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO ()
dialog = putStr "What is your happy number? "
         >> getLine
         >>= \n -> let num = read n :: Int in
                   if num == 7
                   then putStrLn "Ah, lucky 7!"
                   else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"

dialogd :: IO ()
dialogd = do
   putStrLn "Dawaj liczbe xD:"
   n <- getLine
   let result = read n :: Integer
   if result == 7
          then putStrLn "yo"
          else putStrLn "not ok"
		  
echo3d :: IO ()
echo3d = do
      n <- getLine
      l <- getLine
      putStrLn $ n ++ " chciales echo i gunwo xD " ++l
	  

twoQuestionsd :: IO ()
twoQuestionsd = do
  putStr "What is your name? "
  name <- getLine
  putStr "How old are you? "
  age <- getLine
  print (name,age)	

  --nie mam pomyslu jak to inaczej zrobic a to dziala prawdopodobnie
  --w poprzednim nie dzialaly spacje....
twoQuestions :: IO()
twoQuestions = putStrLn "dasz mi to imie?"      >>= \_ ->
               getLine                          >>= \i ->
			   let imie2 = read i :: String in 
               putStrLn "dawaj wiek"            >>= \_ ->
               getLine                          >>= \wiek ->
			   let wiek2 = read wiek :: Integer in
               print(i," masz ",wiek2," lat")
			   
twoQuestionsdd :: IO()
twoQuestionsdd = do
             putStrLn "imie"
             imie <- getLine
             putStrLn "wiek"
             wiek <- getLine
             let wiek2 = read wiek :: Integer in
              print ("imie",imie,"wiek",wiek2)
	  
getLine' :: IO String
getLine' = do
          x <- getChar
          if x == '\n'
          then return []
          else do 
				xs <- getLine'
				return (x:xs)			
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  