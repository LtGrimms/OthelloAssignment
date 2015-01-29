


module TonysOthelloFunctions where

import OthelloTools

-- | validMoves 
{- this function needs to take in a player and a board and return that players valid moves as a list of pairs (Int, Int) -}
validMoves    :: GameState -> [(Int, Int)]
validMoves (GameState {play = p, theBoard = b})
    | fst p == Black = validMoves' b
    | fst p == White = validMoves' (invertBoardPieces b)

{- The real work of validMoves is done here, we can now assume we are looking for valid moves for the black pieces -}
validMoves'   :: Board -> [(Int, Int)]
validMoves' _ = [(1, 1)]


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

