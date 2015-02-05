module IO where

import OthelloTools
import FSM
import TonysOthelloFunctions

---------------------From Othello.hs------------------------------

{- | This is the type for all player functions.  A player strategy function takes a
board and returns a point (Int,Int) that it chooses -- this should be a legal move.
If the player passes, the funciton should return Nothing.
-}
type Chooser = GameState -> Cell -> [Maybe (Int,Int)]

-- | This strategy lives up to it's name: it always chooses to play at cell (0,0).
reallyStupidStrategy  :: Chooser
reallyStupidStrategy b c = [Just(3,5), Just(4, 5)]

-- | This is a sample greedy strategy
greedy :: Chooser
greedy (GameState {play = p, theBoard = b}) c = mapJust (maxCaptures (findMovesAndCaptures b c))

maxCaptures :: [[(Int,Int)]] -> [(Int, Int)]
maxCaptures moves = foldr findMax [] moves

findMax :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
findMax [] [] = []
findMax [] a = a
findMax a b
 | length a > length b = a
 | otherwise = b

mapJust :: [(Int, Int)] -> [Maybe (Int, Int)]
mapJust [] = []
mapJust (x:xs) = Just (fst x, snd x) : mapJust xs

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                    in  (if null zs then (if null ys then [] else init ys) else ys) ++ [elem] ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

---------------------Other Choosers-------------------------------


----------------Strategy Names and Functions----------------------

data Strategy = Stupid | Greedy | St3 | DoesNotExist

instance Show (Strategy) where
  show s = strategy2Strn s

instance Eq (Strategy) where
  s1 == s2 = strategy2Strn s1 == strategy2Strn s2

strategy2Strn   :: Strategy -> String
strategy2Strn Stupid = "Stupid"
strategy2Strn Greedy = "Greedy"
strategy2Strn St3 = "St3"
strategy2Strn DoesNotExist = "This is not a strategy"

strn2Strategy   :: String -> Strategy
strn2Strategy "Stupid" = Stupid
strn2Strategy "Greedy" = Greedy
strn2Strategy "St3" = St3
strn2Strategy _ = DoesNotExist

strategy2Chooser :: Strategy -> Chooser
strategy2Chooser Stupid = reallyStupidStrategy
strategy2Chooser Greedy = greedy

strn2Chooser :: String -> Chooser
strn2Chooser s = strategy2Chooser (strn2Strategy s)

putStrategy     :: Strategy -> IO()
putStrategy s = putStr (strategy2Strn s)

---------------------Playing the game functions--------------------
----------------------Don't put IO in these------------------------

placePiece :: (Int, Int) -> Board -> Cell -> Board
placePiece mv b p = replace2 b ((fst mv - 1), (8 - snd mv)) p

playMove :: [Maybe (Int, Int)] -> Board -> Cell -> Board
playMove [] b _ = b
playMove (mv:mvs) b p
 | mv == Nothing = playMove mvs b p
 | otherwise = placePiece (maybe (0,0) (\x -> x) mv) (playMove mvs b p) p

playedBy :: Player -> Cell
playedBy White = W
playedBy Black = B

newPlayedFrom :: [Maybe (Int, Int)] -> Played
newPlayedFrom [] = Passed
newPlayedFrom moves = Played (maybe (0,0) (\x -> x) (head moves))

unmaybe :: Maybe (Int, Int) -> (Int, Int)
unmaybe = maybe (0, 0) (\y -> y)

unMaybe :: [Maybe (Int, Int)] -> [(Int, Int)]
unMaybe = map unmaybe


nextGamestate :: Chooser -> GameState -> GameState
nextGamestate c (GameState {play = p, theBoard = b}) =
  GameState (player, (newPlayedFrom setOfMoves)) (playMove setOfMoves b playerCell)

  where
    player = invertPlayer (fst p)
    playerCell = playedBy player
    setOfMoves = c (GameState p b) playerCell


firstMove :: Chooser -> GameState -> GameState
firstMove c (GameState {play = p, theBoard = b}) =
  GameState (player, (newPlayedFrom setOfMoves)) (playMove setOfMoves b playerCell)

  where
    player = fst p
    playerCell = playedBy player
    setOfMoves = c (GameState p b) playerCell

-----------------------------main----------------------------------


main = do
  putStrLn "Hello, Please enter the name of Strategy 1"

  let inputChecking a =
	  if (strn2Strategy a /= DoesNotExist)
        then putStr ("valid Strategy " ++ a ++ " selected\n")
	    else do
          putStr "invalid Strategy\n"
          return () -- why doesnt this quit the execution?

  s1 <- getLine
  inputChecking s1

  putStrLn "Please enter the second Strategy"
  s2 <- getLine
  inputChecking s2

  print initBoard
  print (firstMove (strn2Chooser s1) initBoard)

  let playTheGame :: Chooser -> Chooser -> GameState -> IO ()
      playTheGame active inactive (GameState {play = p, theBoard = b})
        | (lastPlay == Passed) && (newSetOfMoves == []) = do
                                                        print "Both players passed, game over"
        | otherwise = do
          print newGameState
          playTheGame inactive active newGameState

         where
          lastPlayer = fst p
          lastPlay = snd p
          currentGameState = GameState p b
          currentPlayer = invertPlayer (fst p)
          newSetOfMoves = active (GameState p b) (playedBy currentPlayer)
          newMove = head (unMaybe newSetOfMoves)
          newGameState = nextGamestate active currentGameState

  playTheGame (strn2Chooser s2) (strn2Chooser s1) (firstMove (strn2Chooser s1) initBoard)






