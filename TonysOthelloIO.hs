module IO where

import OthelloTools
import FSM
import TonysOthelloFunctions
import System.Environment
import System.Exit


---------------------From Othello.hs------------------------------

{- I needed to import these functions so that everything ran properly,
I didnt want to change Othello.hs to avoid merge conflicts -}

{- | This is the type for all player functions.  A player strategy function takes a
board and returns a point (Int,Int) that it chooses -- this should be a legal move.
If the player passes, the funciton should return Nothing.
-}
type Chooser = GameState -> Cell -> [Maybe (Int,Int)]

-- | This strategy lives up to it's name: it always chooses to play at cell (0,0).
reallyStupidStrategy  :: Chooser
reallyStupidStrategy b c = [Just(3,5), Just(4, 5)]

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                    in  (if null zs then (if null ys then [] else init ys) else ys) ++ [elem] ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

---------------------Other Choosers-------------------------------

-----------------GREEDY--------------
-- | This is a sample greedy strategy
greedy :: Chooser
greedy (GameState {play = p, theBoard = b}) c = mapJust (maxCaptures (findAllMovesAndCaptures b c))

-- | helper function for greedy chooser, takes a list of valid moves and returns the one with the
--   most caputures
maxCaptures :: [[(Int,Int)]] -- ^ Takes in the list of valid moves in the form of a nested list of Int pairs
			-> [(Int, Int)] -- ^ Returns the move with the most captures along with the captured pieces
maxCaptures moves = foldr findMax [] moves

-- | helper function for greedy chooser, compares two moves and determines which will capture more
findMax :: [(Int,Int)]  -- ^ First move to be compared
        -> [(Int,Int)]  -- ^ Second move to be compared
        -> [(Int,Int)]  -- ^ Returns the move with most captures

findMax [] [] = []
findMax [] a = a
findMax a b
 | length a > length b = a
 | otherwise = b

-- | since choosers retrun Maybe (Int, Int)s this makes a move into a 'Just' move
mapJust :: [(Int, Int)] -- ^ Takes in a normal list of Int pairs
		-> [Maybe (Int, Int)] -- ^ Returns a casted Just pair of Ints
mapJust [] = []
mapJust (x:xs) = Just (fst x, snd x) : mapJust xs
--------------------------------------



-------------------FirstAvailable----------------------------------------
firstAvailable_st :: Chooser
firstAvailable_st (GameState {play = p , theBoard =b})  c = mapJust(firstAvailable (findAllMovesAndCaptures b c))

firstAvailable :: [[(Int,Int)]] -> ([(Int,Int)])
firstAvailable moves
  | moves == [] = []
  | otherwise = (head (moves))
-------------------------------------------------------------------------


------------------corner--------------------------------------------------
corner_st :: Chooser
corner_st (GameState {play = p , theBoard =b})  c = mapJust( corner (findAllMovesAndCaptures b c))

mysubtract_1  (a,b) = (a-1,b-1) 
mysubtract_2 :: (Int,Int) -> (Int, Int) 
mysubtract_2  (a,b) = (a-1,b-8) 
mysubtract_3 :: (Int,Int) -> (Int, Int) 
mysubtract_3  (a,b) = (a-8,b-1) 
mysubtract_4 :: (Int,Int) -> (Int, Int) 
mysubtract_4  (a,b) = (a-8,b-8) 
mysquare :: (Int,Int) -> Int 
mysquare (a,b) = a^2+b^2 

{- get 4 distances between cell postion and 4 corner position(1,1)(1,8)(8,1)(8,8)-}
getAllDistance :: [(Int,Int)] -> [Int] 
getAllDistance a = [mysquare(mysubtract_1(head(a))),mysquare(mysubtract_2(head(a))),mysquare(mysubtract_3(head(a))),mysquare(mysubtract_4(head(a)))] 

{-find smallest distance among above 4 distances-}
findSmall :: [Int] -> Int
findSmall x = minimum x

{-make array which contain smallest distance and list-}
makeArr :: [(Int,Int)] -> (Int,[(Int,Int)])
makeArr x = (findSmall(getAllDistance x) , x )

{-find all lists' smallest distance and compare which distance is smallest-}
mycalculator :: [(Int, Int)] -> (Int,[(Int,Int)]) -> (Int,[(Int,Int)])
mycalculator moves ((-1),[ ]) = makeArr moves
mycalculator moves (s,x) = if(fst(makeArr moves)>=s) then (s,x) else makeArr moves

corner :: [[(Int,Int)]] -> ([(Int,Int)])
corner x = snd(foldr mycalculator ((-1),[ ]) x)


--------------------------------------------------------------------------

----------------Strategy Names and Functions----------------------

-- | these are the valid strategies
data Strategy = First | Greedy | Corner | DoesNotExist

instance Show (Strategy) where
  show s = strategy2Strn s

instance Eq (Strategy) where
  s1 == s2 = strategy2Strn s1 == strategy2Strn s2

-- | Converts a Strategy data type to a string representation
strategy2Strn   :: Strategy -> String
strategy2Strn First = "First"
strategy2Strn Greedy = "Greedy"
strategy2Strn Corner = "Corner"
strategy2Strn DoesNotExist = "This is not a strategy"

-- | converts a string into a strategy
strn2Strategy   :: String -> Strategy
strn2Strategy "Corner" = Corner
strn2Strategy "Greedy" = Greedy
strn2Strategy "First" = First
strn2Strategy _ = DoesNotExist

-- | converts strategies into their cooresponding chooser types
strategy2Chooser :: Strategy -> Chooser
strategy2Chooser Greedy = greedy
strategy2Chooser First = firstAvailable_st
strategy2Chooser Corner = corner_st

-- | composses strn2Strategy and strategy2Chooser
strn2Chooser :: String -> Chooser
strn2Chooser s = strategy2Chooser (strn2Strategy s)

putStrategy     :: Strategy -> IO()
putStrategy s = putStr (strategy2Strn s)

---------------------Playing the game functions--------------------
----------------------Don't put IO in these------------------------

-- | this will palce an individual piece on a board
placePiece :: (Int, Int)  -- ^ Place to put the piece (in Std coordinates)
           -> Board       -- ^ Current board to place Piece
           -> Cell        -- ^ Represents which player is placing the piece
           -> Board       -- ^ Returns the board with piece played
placePiece mv b p = replace2 b ((fst mv - 1), (8 - snd mv)) p

-- | this will take in an entire move and play/capture each piece
playMove :: [Maybe (Int, Int)] -- ^ The move from a chooser
         -> Board              -- ^ The board to play the move on
         -> Cell               -- ^ The Player making the move
         -> Board              -- ^ Returns the board after the move is made
playMove [] b _ = b
playMove (mv:mvs) b p
 | mv == Nothing = playMove mvs b p
 | otherwise = placePiece (maybe (0,0) (\x -> x) mv) (playMove mvs b p) p

-- | Converts a player Type into is cooresponding Cell type
playedBy :: Player -- ^ Takes in the player that is playing
			-> Cell -- ^ Returns the corresponding representation of the player
playedBy White = W
playedBy Black = B

-- | This function is used to create the 'Played' variable in a new gamestate.
--   It will take in a move from a chooser and determine if it is a pass or where
--   the chooser chose to play
newPlayedFrom :: [Maybe (Int, Int)] -- ^ Takes in a pair of Maybe Ints
				-> Played -- ^ Returns whether or not the move is a pass or play
newPlayedFrom [] = Passed
newPlayedFrom moves = Played (maybe (0,0) (\x -> x) (head moves))


-- | Since moves are of type [Maybe (Int, Int)] we need sometimes need to remove the
--   'maybes'. This function does that.
unMaybe :: [Maybe (Int, Int)] -- ^ Takes in a Maybe pair of Ints
		-> [(Int, Int)] -- ^ Returns a normal pair of Ints
unMaybe = map unmaybe
  where unmaybe :: Maybe (Int, Int) -> (Int, Int)
        unmaybe = maybe (0, 0) (\y -> y)

-- | next gamestate represnts a chooser taking a turn in the game
nextGamestate :: Chooser     -- ^ The chooser taking a turn
              -> GameState   -- ^ The gamestate before the turn is taken
              -> GameState   -- ^ Returns the gamestate after the turn is taken
nextGamestate c (GameState {play = p, theBoard = b}) =
  GameState (player, (newPlayedFrom setOfMoves)) (playMove setOfMoves b playerCell)

  where
    player = invertPlayer (fst p)
    playerCell = playedBy player
    setOfMoves = c (GameState p b) playerCell


-- | since the board starts with a play variable (Black, Init) we need a
--   different function to make the first move that will not flip the Player
--   variable within Play
firstMove :: Chooser -- ^ Takes in the strategy of the first player
		-> GameState -- ^ Takes in the initial gamestate
		-> GameState -- ^ Returns the new gamestate after the move is played
firstMove c (GameState {play = p, theBoard = b}) =
  GameState (player, (newPlayedFrom setOfMoves)) (playMove setOfMoves b playerCell)

  where
    player = fst p
    playerCell = playedBy player
    setOfMoves = c (GameState p b) playerCell

-- | endgame will determine the winner and print out an endgame message
endgame :: String -> String -> Board -> IO ()
endgame c1 c2 b
  | countBlackPieces b > countWhitePieces b = putStrLn ("Black wins! Black (" ++ c1 ++ ") : " ++ show (countBlackPieces b) ++  " White (" ++ c2 ++ ") :" ++ show (countWhitePieces b))
  | countBlackPieces b == countWhitePieces b = putStrLn ("Tied!, Black (" ++ c1 ++ ") : " ++ show (countBlackPieces b) ++  " White (" ++ c2 ++ ") :" ++ show (countWhitePieces b))
  | otherwise                       = putStrLn ("White wins! Black (" ++ c1 ++ ") : " ++ show (countBlackPieces b) ++  " White (" ++ c2 ++ ") :" ++ show (countWhitePieces b))

-- | counts the black pieces on a board
countBlackPieces :: Board -> Int
countBlackPieces [] = 0
countBlackPieces (x:xs) = (countBlack x) + (countBlackPieces xs)
  where countBlack :: [Cell] -> Int
        countBlack [] = 0
        countBlack (x:xs)
         | x == B = 1 + countBlack xs
         | otherwise = 0 + countBlack xs

-- | counts the white pieces on a board
countWhitePieces :: Board -> Int
countWhitePieces b = countBlackPieces (invertBoardPieces b)

-----------------------------main----------------------------------

playTheGame :: Strategy -> Strategy -> GameState -> IO ()
playTheGame active inactive (GameState {play = p, theBoard = b})
      | (lastPlay == Passed) && (newSetOfMoves == []) = do
           if (lastPlayer == Black)
             then endgame strat2String strat1String b
           else endgame strat1String strat2String b
      | otherwise = do
        print newGameState
        playTheGame inactive active newGameState

        where
        lastPlayer = fst p
        lastPlay = snd p
        currentGameState = GameState p b
        currentPlayer = invertPlayer (fst p)
        newSetOfMoves = (strategy2Chooser active) (GameState p b) (playedBy currentPlayer)
        newMove = head (unMaybe newSetOfMoves)
        newGameState = nextGamestate (strategy2Chooser active) currentGameState
        strat1String = strategy2Strn active
        strat2String = strategy2Strn inactive


main = do
    argument	<-	getArgs

    putStrLn "Hello, Welcome to the CPSC449 Othello Assignment"
    putStrLn "Valid Strategies are:"
    putStrLn "  Greedy"
    putStrLn "  First"
    putStrLn "  Corner"

    let inputChecking a =
		if (strn2Strategy a /= DoesNotExist)
			then putStr ("valid Strategy " ++ a ++ " selected\n")
		else do
            putStr "invalid Strategy\n"
            putStrLn "Valid Strategies are:"
            putStrLn "  Greedy"
            putStrLn "  First"
            putStrLn "  Corner"
            exitSuccess

    if length argument == 2
		then do
            let s1 = (head argument)
            let s2 = (argument !! 1)
            inputChecking s1
            inputChecking s2
            print initBoard
            print (firstMove (strn2Chooser s1) initBoard)
            playTheGame (strn2Strategy s2) (strn2Strategy s1) (firstMove (strn2Chooser s1) initBoard)
    else
		if length argument == 0
			then do

                putStrLn "Please select a black strategy"
                s1 <- getLine
                inputChecking s1
                putStrLn "Please select a white strategy"
                s2 <- getLine
                inputChecking s2
                print initBoard
                print (firstMove (strn2Chooser s1) initBoard)
                playTheGame (strn2Strategy s2) (strn2Strategy s1) (firstMove (strn2Chooser s1) initBoard)
		else
			do
                putStrLn "Invalid number of arguments"
                putStrLn "Valid Strategies are:"
                putStrLn "  Greedy"
                putStrLn "  First"
                putStrLn "  Corner"
                exitSuccess


-- BUCKET LIST
{-

1. make the program print strategies and quit when invalid strat is input - check
2. make choosers able to see the whole set of moves - check
3. compress moves from findAllMovesAndCaptures - check
4. print winner if game ends with a set of valid moves

-}
