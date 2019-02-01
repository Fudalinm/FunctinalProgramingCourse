nTimes :: Int -> IO () -> IO ()
nTimes 0 action = return ()
nTimes n action = do
  action
  nTimes (n-1) action

ioActionFactory :: Int -> String -> IO ()
ioActionFactory n = case n of
  1 -> \name -> putStrLn ("Good morning, " ++ name)
  2 -> \name -> putStrLn ("Good afternoon, " ++ name)
  3 -> \name -> putStrLn ("Good night, " ++ name)
  _ -> \name -> putStrLn ("Hello, " ++ name)

actionList :: [IO ()]
actionList = [ioActionFactory 1 "Ben",
              ioActionFactory 2 "Joe",
              ioActionFactory 3 "Ally"]

sequence'        :: [IO ()] -> IO ()
sequence' []     =  return ()
sequence' (a:as) =  do a
                       sequence' as
					   
--JAK TO ZROBIC??					   
--Zadania:
--1Napisać odpowiednik sequence' wykorzystujący foldr foldr (>>) (return()) actionList
--2Zmienić postać 1. agrumentu foldr: z >> na wyrażenie lambda
--3Napisać odpowiednik sequence' wykonujący ‘akcje’ od ostaniej 
--do pierwszej; rozważyć co najmniej dwa warianty, np. foldr na 
--odwróconej liście i wykorzystanie foldl --oba na odwroconej liscie czyli foldr (>>) (return()) (reverse $ actionList)				

--NIE WIEM JAK PRZEPISAC NA LAMBDE ale chodzi o to ze
--jesli masz IO to wykonaj na nim f jesli nie masz to nic nie rob...