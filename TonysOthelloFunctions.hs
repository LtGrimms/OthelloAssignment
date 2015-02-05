


module TonysOthelloFunctions where

import OthelloTools



{- Most of these functions are depreciated, the only usefull ones are the rotate functions -}

-- | validMoves 
{- this function needs to take in a player and a board and return that players valid moves as a list of pairs (Int, Int) -}
validMoves    :: GameState -> [(Int, Int)]
validMoves (GameState {play = p, theBoard = b})
    | fst p == Black = validMoves' b
    | fst p == White = validMoves' (invertBoardPieces b)

{- The real work of validMoves is done here, we can now assume we are looking for valid moves for the black pieces -}
validMoves'   :: Board -> [(Int, Int)]
validMoves' x = validMoves'' x ++ validMoves''(rotateX x 1) ++ validMoves''(rotateX x 2) ++ validMoves''(rotateX x 3) ++ validMoves''(rotateX x 4) ++ validMoves''(rotateX x 5) ++ validMoves''(rotateX x 6) ++ validMoves''(rotateX x 7) 

validMoves'' :: Board -> [(Int, Int)]
validMoves'' [] = []
validMoves'' (x:xs) = testRow x (8 - length xs)  ++ validMoves'' xs 

testRow :: [Cell] -> Int -> [(Int, Int)]
testRow [] row = []
testRow x row 
	|fsmPass(foldr fsm (-1) x) = [((length x), row)]  ++ testRow (init x) row
	|otherwise = testRow (init x) row

-- | invertBoardPieces
{- takes a board and returns the board with every piece flipped -}
invertBoardPieces :: Board -> Board
invertBoardPieces [] = []
invertBoardPieces (x:xs) = (invertRowPieces x) : (invertBoardPieces xs)

-- | invertRowPieces
{- subrutine of invertBoardPieces -}
invertRowPieces :: [Cell] -> [Cell]
invertRowPieces [] = []
invertRowPieces (x:xs) = (otherCell x) : (invertRowPieces xs)

-- | fsm
{- this checks if a certian space on a row is a valid move for a black piece.
   States correspond to having seen certian elemtns on the way accross a row
   0 - start state, depreciated
   1 - saw empty square
   2 - saw emprty square followed by white square
   3 - saw black square followed by white squares followed empty square (final state)
   4 - failed state (saw a non empty first || black after empty || empty after white)

   this is ment to be the function fed into a fold function
-}
fsm           :: Cell -> Int -> Int
fsm E (-1) = 1  -- calls to the fsm should start with a (-1) entry to initiallize
fsm _ (-1) = 4
fsm _ 4 = 4     -- always stays in a failed state
fsm _ 3 = 3     -- always stays in passed state
--fsm E 0 = 1     -- move from start state to state 1
--fsm B 0 = 4     -- fails on black first
--fsm W 0 = 4     -- fails on white first
fsm B 1 = 4     -- fails on black after empty
fsm E 1 = 4     -- fails on empty after white
fsm W 1 = 2     -- moves to state 2 after white after empty
fsm E 2 = 4     -- fails if empty on state 2
fsm W 2 = 2     -- stays in 2 on white
fsm B 2 = 3     -- passes to complete state on black

-- | fsmPass
-- simple test to see if the fsm passed or failed
fsmPass :: Int -> Bool
fsmPass 4 = False
fsmPass 3 = True
fsmPass _ = False  -- I think I'll need this to ensure the pass func works on full rows



-- fsmValidMoves :: Cell -> Int
-- fsmValidMoves


-- | captureCells  
{- this function needs to take in a row of cells and a pair representing the most recently placed piece and return the row with cells caputred -}

-- captureCells  :: [Cell] -> (Int, Int) -> [Cell]



-- | meeting
-- playMove :: GameState -> (Int,Int) -> GameState


rotateX :: [[a]]-> Int -> [[a]]
rotateX board 0 = board
rotateX board x = rotateX (rotate45CW board) (x-1)

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
             [h1]]       
	