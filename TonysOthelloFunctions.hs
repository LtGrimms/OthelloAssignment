


module TonysOthelloFunctions where

import OthelloTools

-- | validMoves 
{- this function needs to take in a player and a board and return that players valid moves as a list of pairs (Int, Int) -}
validMoves    :: GameState -> (Int, Int)
validMoves (GameState {play = p, theBoard = b})
    | fst p == Black = validMoves' b
    | fst p == White = validMoves' (invertBoardPieces b)

{- The real work of validMoves is done here, we can now assume we are looking for valid moves for the black pieces -}
validMoves'   :: Board -> (Int, Int)
validMoves' _ = (1, 1)


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




-- | captureCells  
{- this function needs to take in a row of cells and a pair representing the most recently placed piece and return the row with cells caputred -}

-- captureCells  :: [Cell] -> (Int, Int) -> [Cell]

