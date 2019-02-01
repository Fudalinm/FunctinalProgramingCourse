import System.Environment
import System.IO
import Data.Char(toUpper)
-- openFile :: FilePath -> IOMode -> IO Handle

{-
powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r
  | e mod 2 == 1 = powm (b * b mod m) (e div 2) m (r * b mod m)
powm b e m r = powm (b * b mod m) (e div 2) m r
 
main :: IO ()
main =
  print $
  powm
    2988348162058574136915891421498819466320163312926952423791023078876139
    2351399303373464486466122544523690094744975233415544072992656881240319
    (10 ^ 100)
    1
-}




main = do
	print "podaj sciezkud do odczytu \n"
	inFileName <- getLine 
	print "podaj sciezkud do zapisu\n"
	outFileName <- getLine
	inHdlr <- openFile inFileName ReadMode
	outHdlr <- openFile outFileName WriteMode
	inpStr <- hGetContents inHdlr
	putStr ( "tutaj bedzie ten plik \n" ++ inpStr )
	putStr ("\n\n\n\n\n\n\n\n")
	let x =hue2 inpStr 
	putStr x
	hPutStr outHdlr (map toUpper (inpStr ++ gibNullCharStr 1))
	hClose inHdlr
	hClose outHdlr

hue = do
	print "podaj sciezkud do odczytu \n"
	inFileName <- getLine 
	print "podaj sciezkud do zapisu\n"
	outFileName <- getLine
	inHdlr <- openFile inFileName ReadMode
	outHdlr <- openFile outFileName WriteMode
	inpStr <- hGetContents inHdlr
	let x = inpStr

	
	hPutStr outHdlr (map toUpper inpStr)
	hClose inHdlr
	hClose outHdlr

hue2 :: String -> String
hue2 [] = ""
hue2 (x:xs) = (show(fromEnum x)) ++ ":" ++ hue2 xs

gibNullCharStr :: a -> String
gibNullCharStr hue = [(toEnum 0 )]


f :: Integer -> String ->  Integer
f acc [] = acc `div` 256
f acc (x:xs) =  f ((acc + (toInteger (fromEnum x)))* 256) xs