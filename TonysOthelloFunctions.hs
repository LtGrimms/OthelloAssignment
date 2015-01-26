--| validMoves 
-{ this function needs to take in a player and a board and return that players valid moves as a list of pairs (Int, Int) }-

validMoves    :: GamesState -> (Int, Int)

--| captureCells  
-{ this function needs to take in a row of cells and a pair representing the most recently placed piece and return the row with cells caputred }-

captureCells  :: [Cell] -> (Int, Int) -> [Cell]


--| rotate45CCW
--| rotates the board 45deg counter clockwise

rotate45CCW   :: [[a]] -> [[a]]
rotate45CCW [[a1, a2, a3 ,a4, a5, a6, a7, a8],
             [b1, b2, b3, b4, b5, b6, b7, b8],
             [c1, c2, c3, c4, c5, c6, c7, c8],
             [d1, d2, d3, d4, d5, d6, d7, d7],
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

--| rotate45CW
-- this function should only be used to move an already rotated board back to its original pos.

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

