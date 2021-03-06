{- | This module is used for CPSC 449 for the Othello assignment.

You MUST use this file as part of your assignment to deal with boards, 
cells, etc.  This will may be tested
by linking your assignment against a modified version of this file.

Do not modify this file.

Copyright: Copyright 2015, Rob Kremer (rkremer@ucalgary.ca), University of Calgary. 
Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose is hereby granted without fee, provided
that the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation. The University of Calgary makes no representations about the
suitability of this software for any purpose. It is provided "as is" without
express or implied warranty.

-}

module FSM where
import TonysOthelloFunctions (rotateX)
import OthelloTools

------------------------Running FSM on a board-----------------------------

--validMovesOnBoard :: Board -> [(Int, Int)]
-- can't write this yet since we will need the unrotate function

-- | findMoves takes a board and returns only the squares to place pieces
--   This is currently not being used by the main program
findMoves :: Board          -- ^ The board to find moves on
          -> [(Int, Int)]   -- ^ Returns a list of valid squares to move on
findMoves [] = []
findMoves x = map head (findMovesAndCaptures' x)


-- | findMovesAndCaptures take a board and cell and returns each valid move
--   along with the cells that will be captured if that move is played
findMovesAndCaptures :: Board          -- ^ The board to find moves on
                     -> Cell           -- ^ The player cell whose moves will be found
                     -> [[(Int, Int)]] -- ^ Returns the list of moves and captures
findMovesAndCaptures b cell
  | cell == W = findMovesAndCaptures' (invertBoardPieces b)
  | otherwise = findMovesAndCaptures' b


-- | findMovesAndCaptures' is a helper function for findMovesAndCaptures
--   it returns all valid moves and captures for the black pieces on a board
findMovesAndCaptures' :: Board          -- ^ The board to find moves on
                      -> [[(Int, Int)]] -- ^ Returns the list of moves and captures
findMovesAndCaptures' [] = []
findMovesAndCaptures' (x:xs) = movesAndCapturesOnRow (succ (length xs)) (runFSML x) ++ findMovesAndCaptures' xs


-- | findAllMovesAndCaptures returns all valid moves on the board as a double nested list where the "lowest" list
--   contains a move and a list of pieces which that move will capture e.g. [[move, captures ...]]

findAllMovesAndCaptures :: Board -- ^ The board to find moves on
                        -> Cell  -- ^ The cell of the player whose moves we are searching for
			-> [[(Int, Int)]] -- ^ Returns the list of moves and captures
findAllMovesAndCaptures b c
  | c == W = compress (findAllMovesAndCaptures' (invertBoardPieces b))
  | otherwise = compress (findAllMovesAndCaptures' b)
     where compress :: [[(Int,Int)]] -> [[(Int,Int)]]
           compress a = compressAllMovesAndCaptures (compressAllMovesAndCaptures a)

-- | findAllMovesAndCaptures' will assume that we are looking for black pieces and find all moves and
--   captures for the black player.
findAllMovesAndCaptures' :: Board -- ^ The board to find moves on
                         -> [[(Int, Int)]] -- ^ Returns the list of moves and captures
findAllMovesAndCaptures' board = [elem| perm<-[ map(map (`mapMoves` r))(findMovesAndCaptures' (rotateX board r )) | r  <- [0,1,2,3]], elem<-perm]

-- | This is currently not used in any other functions or the execution of the main program
--   This might be usefull in speeding things up if you make it without making calls to movesandcaptures
validMovesOnRow :: [[(Int, Int)]] -- ^ Takes in a nested list of coordinates representing the board
				 -> [(Int,Int)] -- ^ Returns a list of valid moves in a row
validMovesOnRow [] = []
validMovesOnRow (x:xs) = head x : validMovesOnRow xs

-- | This is a helper function for findMoves and Captures, it uses the info from the FSM to create valid (Int, Int) pairs
--   returns the valid moves
movesAndCapturesOnRow :: Int               -- ^ The value of the row fed to the fucntion. Rows are counted upwards from the bottom of a board
                      -> [(Int, Int, Int)] -- ^ The memory from the FSM on the row above
                      -> [[(Int, Int)]]    -- ^ Returns a list of valid moves and the spaces they will capture
movesAndCapturesOnRow _ [] = []
movesAndCapturesOnRow x (y:ys) = (makeSetofCaptures x y True) : movesAndCapturesOnRow x ys
    where makeSetofCaptures :: Int -> (Int, Int, Int) -> Bool -> [(Int, Int)]
          makeSetofCaptures row (col, right, left) first
            | first = (col, row) : makeSetofCaptures row (col, right, left) False
            | (left + right) == 0 = []
            | left == 0 = (col + right, row) : makeSetofCaptures row (col, pred right, 0) False
            | right == 0 = (col - left, row) : makeSetofCaptures row (col, 0, pred left) False
            | otherwise = (col - left, row) : (col + right, row) : makeSetofCaptures row (col, pred right, pred left) False



-- | invertBoardPieces
-- takes a board and returns the board with every piece flipped

invertBoardPieces :: Board -- ^ Takes in the current board
					-> Board -- ^ Returns a board with all the pieces flipped

invertBoardPieces [] = []
invertBoardPieces (x:xs) = (invertRowPieces x) : (invertBoardPieces xs)

-- | invertRowPieces
-- subrutine of invertBoardPieces

invertRowPieces :: [Cell] -- ^ Takes in the list of piece owners in a row
				-> [Cell] -- ^ Returns the list of inverted piece owners in a row

invertRowPieces [] = []
invertRowPieces (x:xs) = (otherCell x) : (invertRowPieces xs)

-- | mapMoves will take an (Int, Int) pair from a board rotated 45, 90 or 135 degrees
--   and return its co-ordinates in standard position

mapMoves :: (Int, Int)   -- ^ The input pair to be 'back-rotated' to stnd position
         -> Int          -- ^ The number of 45 degree rotations to take the input through
         -> (Int, Int)   -- ^ Returns the pair represented in standard position

mapMoves move 0 = move
mapMoves (c,r) 1
	 |(r <= 8) = (8-(r-c),c) 
	 |otherwise = (c,(r-8)+c)
mapMoves (c,r) 2 = (9-r, c)
mapMoves (c,r) 3
	 |(r <= 8) = (9-c, 8 -(r-c))
	 |otherwise = (17-(r+c), c)



-- | compressAllMovesAndCaptures will take in a set of moves and captures from findAll and
--   return a compress version of the set. This in neccessary because captures will be missed
--   if the choosers pick a moves from an uncompressed version. For example if
--   [[(5,4)(5,5)],[(5,4),(6,4)]] is returned from the findAll function the chooser would only
--   get one of the captures from a move that should capture two.
compressAllMovesAndCaptures :: [[(Int,Int)]] -- ^ Takes in the list of moves
                            -> [[(Int,Int)]] -- ^ Returns the compressed list
compressAllMovesAndCaptures []     = []
compressAllMovesAndCaptures (d:[]) = d:[]
compressAllMovesAndCaptures (a:as) = compress a as : compressAllMovesAndCaptures (modified a as)
  where compress :: [(Int, Int)] -> [[(Int, Int)]] -> [(Int, Int)]
        compress a [] = a
        compress a (b:bs)
         | head a == head b = head b : (tail a ++ tail b)
         | otherwise        = compress a bs
        modified :: [(Int,Int)] -> [[(Int,Int)]] -> [[(Int,Int)]]
        modified a [] = []
        modified a (b:bs)
         | head a == head b = bs
         | otherwise        = b : modified a bs





----------------------------------FSM--------------------------------------

-- | State is the thing that an FSM will hold onto as it is folded across a list
--   it represents two seperate finite state machines and a memory bank to store
--   valid moves and thier captures.
--   The first two elements (Int, (Int,Int), _, _, _) represent the fsm tracking moves
--   that will capture pieces to the right of a given square, the first Int will keep
--   track of whether or not a place is valid the (Int, Int) pair will keep track of how
--   many pieces are captured and from which place in the row.
--   The next two elements are identical except they keep track of pieces captured to the
--   left of a given square.
--   The final (Int, Int, Int) tuple repreesents a type of memory and means
--   (Int -- this spot, Int -- will capture this many pieces to the right, Int --  and
--   this many pieces to the left)
type State = (Int, (Int, Int), Int, (Int,Int), [(Int, Int, Int)])


-- | fsm
--   the fsm is designed to be the function used in a call to foldr
fsm :: Cell       -- current cell be examined by fsm
       -> State   -- state before examining cell
       -> State   -- state after examining cell
fsm E (-1, _, _, _, _) = fsmInit E
fsm B (-1, _, _, _, _) = fsmInit B
fsm W (-1, _, _, _, _) = fsmInit W
fsm E s = fsmLeftCalc E (fsmRightCalc E s)
fsm B s = fsmLeftCalc B (fsmRightCalc B s)
fsm W s = fsmLeftCalc W (fsmRightCalc W s)


-- | fsmInit must be called before fsm to make sure the State is initiallized properly
fsmInit :: Cell  -- the first cell in a row
        -> State -- the state after examining the first cell
fsmInit E = (1, (1, 0), 0, (1, 0), [])
fsmInit B = (0, (1, 0), 1, (1, 0), [])
fsmInit W = (0, (1, 0), 0, (1, 0), [])


-- | this controls the manipulation of the first two elements in the State variable
--   and adds to memory when neccessary
fsmRightCalc :: Cell -> State -> State
fsmRightCalc E (rs, (rightCapturesFrom, rightCaptures), x, y, z) = (1, (succ rightCapturesFrom, 0), x, y, z)
fsmRightCalc W (rs, (rightCapturesFrom, rightCaptures), x, y, z)
    | rs == 0 = (0, (succ rightCapturesFrom, 0), x, y, z)
    | rs == 1 = (2, (rightCapturesFrom, 1), x, y, z)
    | rs == 2 = (2, (rightCapturesFrom, succ rightCaptures), x, y, z)
fsmRightCalc B (rs, (rightCapturesFrom, rightCaptures), x, y, z)
    | rs == 0 = (0, (succ rightCapturesFrom, 0), x, y, z)
    | rs == 1 = (0, (succ rightCapturesFrom, 0), x, y, z)
    | rs == 2 = (0, (succ (rightCapturesFrom + rightCaptures), 0), x, y, (rightCapturesFrom, rightCaptures, 0) : z)

-- | this controls the manipulation of the second two elements of the state variable
--   and adds to memory when neccessary
fsmLeftCalc :: Cell -> State -> State
fsmLeftCalc B (x, y, ls, (leftCapturesFrom, leftCaptures), z) = (x, y, 1, (succ leftCapturesFrom, 0), z)
fsmLeftCalc W (x, y, ls, (leftCapturesFrom, leftCaptures), z)
    | ls == 0 = (x, y, 0, (succ leftCapturesFrom, leftCaptures), z)
    | ls == 1 = (x, y, 2, (leftCapturesFrom, succ leftCaptures), z)
    | ls == 2 = (x, y, 2, (leftCapturesFrom, succ leftCaptures), z)
fsmLeftCalc E (x, y, ls, (leftCapturesFrom, leftCaptures), z)
    | ls == 0 = (x, y, 0, (succ leftCapturesFrom, leftCaptures), z)
    | ls == 1 = (x, y, 0, (succ leftCapturesFrom, leftCaptures), z)
    | ls == 2 = (x, y, 0, (newMark, 0), (newMark, 0, leftCaptures): z)
    where newMark = succ (leftCapturesFrom + leftCaptures)

-- | since the fsm's memory is not in a nice format once the fsm finishes this will help
--   clean it up. The first four elements of state are remoeved and only the memory will remain
--   with a compressed version of the valid moves
fsmMemManage :: State -> [(Int, Int, Int)]
fsmMemManage (x, y, z, a, mem) = compress mem
    where compress :: [(Int, Int, Int)] -> [(Int, Int, Int)]
          compress [] = []
          compress (x:[]) = (x:[])
          compress (x:xs:xss)
            | (first x) == (first xs) = (x `merge` xs) : compress xss
            | otherwise = (x : (compress (xs:xss)))


-- | merge is a helper function for fsmMemManage
merge :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
merge (x1, x2, x3) (y1, y2, y3)
    | x2 == 0 = (x1, y2, y3)
    | otherwise = (x1, x2, y3)

-- | finds the first in a 3-tuple
first :: (Int, Int, Int) -> Int
first (f, _, _) = f

-- | this is syntactic sugar that allows you to run the fsm without typeing so much
--  just type runFSM row with any row and the compressed memory will be returned
--  THIS IS NOT USED IN THE MAIN PROGRAM AS FSM SHOULD BE RUN FROM THE LEFT
runFSM :: [Cell]             -- the row to run the FSM on
       -> [(Int, Int, Int)]  -- the cempressed memory of the fsm
runFSM row = fsmMemManage (foldr fsm (-1, (0,0), -1, (0,0), []) row)


-- | this switches the fsm from being a function plugged into a foldl function to
--  being a function plugged into a foldr function
fsml :: State -> Cell -> State
fsml x y = fsm y x

-- | this is syntactic sugar that allows you to run the fsm without typeing so much
--  just type runFSML row with any row and the compressed memory will be returned
--  THIS IS THE VERSION OF FSM TO USE
runFSML  :: [Cell]             -- the row to be tested
         -> [(Int, Int, Int)]  -- the compressed memory
runFSML row = fsmMemManage (foldl fsml (-1, (0,0), -1, (0,0), []) row)


