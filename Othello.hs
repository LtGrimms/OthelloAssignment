import Debug.Trace 
import Control.Monad.Trans.State.Lazy
import Data.Maybe 
import System.Random
import Data.List
import System.Environment
import System.IO.Unsafe
import Data.Either
import OthelloTools
import TonysOthelloFunctions
import FSM
--need some help which validmove function should I be using one from TonysOthelloFunctions or fsm or the playmove commented out in this file

{- | This program is used as a template for the CPSC 449  Othello assignment.

Feel free to modify this file as you see fit.

Copyright: Copyright 2015, Rob Kremer (rkremer@ucalgary.ca), University of Calgary. 
Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose is hereby granted without fee, provided
that the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation. The University of Calgary makes no representations about the
suitability of this software for any purpose. It is provided "as is" without
express or implied warranty.

-}

---Main-------------------------------------------------------------

main = main' (unsafePerformIO getArgs)
    
{- | We have a main' IO function so that we can either:
     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args = do
{-preparing to randomize the strategy
    let randomize = randomRIO (0, 8) :: IO Int
    r1 <- randomize 
    r2 <- randomize
	-}
{-seeding random number generator
	g <- getStdGen
	let x = randoms g :: [Int]
	-}
--taking arguments from command land
    argument	<-	getArgs
    let inputChecking a =
	    if length argument /= 2 -- if the user did not enter any arguments go to interactive mode
	        then do 
	            putStrLn "The number of arguments are incorrect"
	            putStrLn "  reallyStupidStrategy\n"
	            putStrLn "  jasonStrategy\n"
	            putStrLn "Please enter strategy 1 \n"
	            strategy1	<- getLine --getting user input
	            putStrLn "Please enter strategy 2 \n"
	            strategy2	<- getLine --getting user input
	            putStrLn ("The strategies you pick are: " ++ strategy1 ++ " " ++ strategy2)
	    else 
	        putStrLn "The correct number of args"
	        
    inputChecking argument
       
{-seeding random number generator
	g <- getStdGen
	let x = randoms g :: [Int]
	-}
	
    putStrLn ("You gave " ++ show (length args) ++ " arguments")
    putStrLn "\nThe initial board:"
    print initBoard
    
    putStrLn "The initial board rotated 90 degrees:"
    putBoard $ rotateClock $ theBoard initBoard
    
    putStrLn "\nThe initial board with reallyStupidStrategy having played one move (clearly illegal!):"
    let mv = reallyStupidStrategy (initBoard) B
       in case mv of
          Nothing   -> putStrLn "Black passed."
          (Just pt) -> putBoard $ replace2 (theBoard initBoard) pt B

---Strategies-------------------------------------------------------

{- | This is the type for all player functions.  A player strategy function takes a 
     board and returns a point (Int,Int) that it chooses -- this should be a legal move.  
     If the player passes, the funciton should return Nothing.
-}           
type Chooser = GameState -> Cell -> Maybe (Int,Int)

-- | This strategy lives up to it's name: it always chooses to play at cell (0,0).
reallyStupidStrategy  :: Chooser
reallyStupidStrategy b c = Just (0,0)

{- |Strategy to pick the first valid move in the list
	Uses code from Tony's greedy strategy to cast the move as a Just move

pickFirst :: Chooser -- ^ Takes in a Chooser (which returns a Maybe (Int,Int))
pickFirst (GameState {play = p, theBoard = b}) c
	|length findAllMovesAndCaptures == 0 = Nothing
	|length findAllMovesAndCaptures /= 0 = mapJust (head (head (findMovesAndCaptures b c)))

--	|Random strategy that chooses a random move contained from all valid moves
randomStrategy :: Int -- ^ Takes in an Int representing the random number
				-> Chooser -- ^ Takes in a Chooser (which returns a Maybe (Int,Int))
randomStrategy random (GameState {play = p, theBoard = b}) c
	|length findAllMovesAndCaptures == 0 = Nothing
	|length findAllMovesAndCaptures /= 0 = mapJust (pickRandom (findMovesAndCaptures b c))

-}

--	|Used to pick a random move from the total set of valid moves
pickRandom :: [[(Int,Int)]] -- ^ Takes in a set of a set of pairs from FindMovesAndCaptures, represents all valid moves
			-> Int -- ^ Takes in the random number as an Int
			-> [(Int,Int)] -- ^ Returns a chosen move as well as captures from that move
pickRandom list rand = list !! (rand `mod` ((length list) - 1))


jasonStrategy	:: Chooser
jasonStrategy b c 
	|length (findAllMovesAndCaptures (theBoard b) ) /= 0  = Just (head(head( findAllMovesAndCaptures (theBoard b) )))
	|True = Just (0,0)

---Board rotations-------------------------------------------------------------

-- | Rotate a board 90 degrees clockwise.
rotateClock     :: [[a]] -> [[a]]
rotateClock       [ [a0, a1, a2, a3, a4, a5, a6, a7],
                    [b0, b1, b2, b3, b4, b5, b6, b7],
                    [c0, c1, c2, c3, c4, c5, c6, c7],
                    [d0, d1, d2, d3, d4, d5, d6, d7],
                    [e0, e1, e2, e3, e4, e5, e6, e7],
                    [f0, f1, f2, f3, f4, f5, f6, f7],
                    [g0, g1, g2, g3, g4, g5, g6, g7],
                    [h0, h1, h2, h3, h4, h5, h6, h7] ] =
                  [ [h0, g0, f0, e0, d0, c0, b0, a0],
                    [h1, g1, f1, e1, d1, c1, b1, a1],
                    [h2, g2, f2, e2, d2, c2, b2, a2],
                    [h3, g3, f3, e3, d3, c3, b3, a3],
                    [h4, g4, f4, e4, d4, c4, b4, a4],
                    [h5, g5, f5, e5, d5, c5, b5, a5],
                    [h6, g6, f6, e6, d6, c6, b6, a6],
                    [h7, g7, f7, e7, d7, c7, b7, a7] ]

-- | Rotate a board 90 degrees counter clockwise.
rotateCounter     [ [h0, g0, f0, e0, d0, c0, b0, a0],
                    [h1, g1, f1, e1, d1, c1, b1, a1],
                    [h2, g2, f2, e2, d2, c2, b2, a2],
                    [h3, g3, f3, e3, d3, c3, b3, a3],
                    [h4, g4, f4, e4, d4, c4, b4, a4],
                    [h5, g5, f5, e5, d5, c5, b5, a5],
                    [h6, g6, f6, e6, d6, c6, b6, a6],
                    [h7, g7, f7, e7, d7, c7, b7, a7] ] =
                  [ [a0, a1, a2, a3, a4, a5, a6, a7],
                    [b0, b1, b2, b3, b4, b5, b6, b7],
                    [c0, c1, c2, c3, c4, c5, c6, c7],
                    [d0, d1, d2, d3, d4, d5, d6, d7],
                    [e0, e1, e2, e3, e4, e5, e6, e7],
                    [f0, f1, f2, f3, f4, f5, f6, f7],
                    [g0, g1, g2, g3, g4, g5, g6, g7],
                    [h0, h1, h2, h3, h4, h5, h6, h7] ]

-- | rotate45CCW
-- | rotates the board 45deg counter clockwise

rotate45CCW   :: [[a]] -> [[a]]
rotate45CCW [[a1, a2, a3 ,a4, a5, a6, a7, a8],
             [b1, b2, b3, b4, b5, b6, b7, b8],
             [c1, c2, c3, c4, c5, c6, c7, c8],
             [d1, d2, d3, d4, d5, d6, d7, d8],
             [e1, e2, e3, e4, e5, e6, e7, e8],
             [f1, f2, f3, f4, f5, f6, f7, f8],
             [g1, g2, g3, g4, g5, g6, g7, g8],
             [h1, h2, h3, h4, h5, h6, h7, h8]] =
            [[a8],
             [a7, b8],
             [a6, b7, c8],
             [a5, b6, c7, d8],
             [a4, b5, c6, d7, e8],
             [a3, b4, c5, d6, e7, f8],
             [a2, b3, c4, d5, e6, f7, g8],
             [a1, b2, c3, d4, e5, f6, g7, h8],
             [b1, c2, d3, e4, f5, g6, h7],
             [c1, d2, e3, f4, g5, h6],
             [d1, e2, f3, g4, h5],
             [e1, f2, g3, h4],
             [f1, g2, h3],
             [g1, h2],
             [h1]]
rotate45CCW [[a8],
             [a7, b8],
             [a6, b7, c8],
             [a5, b6, c7, d8],
             [a4, b5, c6, d7, e8],
             [a3, b4, c5, d6, e7, f8],
             [a2, b3, c4, d5, e6, f7, g8],
             [a1, b2, c3, d4, e5, f6, g7, h8],
             [b1, c2, d3, e4, f5, g6, h7],
             [c1, d2, e3, f4, g5, h6],
             [d1, e2, f3, g4, h5],
             [e1, f2, g3, h4],
             [f1, g2, h3],
             [g1, h2],
             [h1]]=
            [[a8, b8, c8, d8, e8, f8, g8, h8], 
             [a7, b7, c7, d7, e7, f7, g7, h7],
             [a6, b6, c6, d6, e6, f6, g6, h6],
             [a5, b5, c5, d5, e5, f5, g5, h5],
             [a4, b4, c4, d4, e4, f4, g4, h4],
             [a3, b3, c3, d3, e3, f3, g3, h3],
             [a2, b2, c2, d2, e2, f2, g2, h2],
             [a1, b1, c1, d1, e1, f1, g1, h1]] 
             

-- | rotate45CW
-- this function should only be used to move an already rotated board back to its original pos.
{--
rotate45CW    :: [[a]] -> [[a]]
rotate45CW [[a8],
            [a7, b8],
            [a6, b7, c8],
            [a5, b6, c7, d8],
            [a4, b5, c6, d7, e8],
            [a3, b4, c5, d6, e7, f8],
            [a2, b3, c4, d5, e6, f7, g8],
            [a1, b2, c3, d4, e5, f6, g7, h8],
            [b1, c2, d3, e4, f5, g6, h7],
            [c1, d2, e3, f4, g5, h6],
            [d1, e2, f3, g4, h5],
            [e1, f2, g3, h4],
            [f1, g2, h3],
            [g1, h2],
            [h1]] =
           [[a1, a2, a3 ,a4, a5, a6, a7, a8],
            [b1, b2, b3, b4, b5, b6, b7, b8],
            [c1, c2, c3, c4, c5, c6, c7, c8],
            [d1, d2, d3, d4, d5, d6, d7, d7],
            [e1, e2, e3, e4, e5, e6, e7, e8],
            [f1, f2, f3, f4, f5, f6, f7, f8],
            [g1, g2, g3, g4, g5, g6, g7, g8],
            [h1, h2, h3, h4, h5, h6, h7, h8]]

rotate45CW  [[a8, b8, c8, d8, e8, f8, g8, h8], 
             [a7, b7, c7, d7, e7, f7, g7, h7],
             [a6, b6, c6, d6, e6, f6, g6, h6],
             [a5, b5, c5, d5, e5, f5, g5, h5],
             [a4, b4, c4, d4, e4, f4, g4, h4],
             [a3, b3, c3, d3, e3, f3, g3, h3],
             [a2, b2, c2, d2, e2, f2, g2, h2],
             [a1, b1, c1, d1, e1, f1, g1, h1]]=
            [[a8],
             [a7, b8],
             [a6, b7, c8],
             [a5, b6, c7, d8],
             [a4, b5, c6, d7, e8],
             [a3, b4, c5, d6, e7, f8],
             [a2, b3, c4, d5, e6, f7, g8],
             [a1, b2, c3, d4, e5, f6, g7, h8],
             [b1, c2, d3, e4, f5, g6, h7],
             [c1, d2, e3, f4, g5, h6],
             [d1, e2, f3, g4, h5],
             [e1, f2, g3, h4],
             [f1, g2, h3],
             [g1, h2],
             [h1]]       --}
-- | These functions return a rotated point the same as the board rotations above.
type PointRotation = (Int,Int) -> (Int,Int)

-- | rotate 90 degrees clockwise.
rotatePt             :: PointRotation
rotatePt             (x,y) = (7-y, x)

---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                    in  (if null zs then (if null ys then [] else init ys) else ys) ++ [elem] ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)
{--
playMove :: GameState -> (Int, Int) -> GameState
playMove gs mv
         newBoard = replace2 board mv
	 --flip the other cells
         where board = theBoard gs
--}
{--
rotateX :: [[a]]-> Int -> [[a]]
rotateX board 0 = board
rotateX board x = rotateX (rotate45CW board (x-1))--}

---Flipping Functions -------------------------

--	|Used to flip a single cell
flip' :: Board -- ^ Takes in the current board
		-> (Int,Int) -- ^ Takes in the coordinate of the piece you want to flip
		-> Cell -- ^ Takes in the player
		-> Board -- ^ Returns the new board
flip' board pt player = replace2 board (convert pt) player


--	|Have to pass in the tail of the validMovesAndCaptures
flipList :: Board -- ^ Takes in the current board
		-> [(Int,Int)] -- ^ Takes in a list of coordinates to flip
		-> Cell -- ^ Takes in a player
		-> Board -- ^ Returns the new board
flipList a (x:xs) player = flipList (flip' a x player) xs player

--	|Have to convert between the coordinates returned by findMovesAndCaptures in order to use the replace function
convert :: (Int,Int) -- ^ Takes in a pair which represents the initial cooridnates
		-> (Int,Int) -- ^ Returns the converted pair
convert x = (((snd x) - 1), (abs ((fst x) - 8)))
