module FSM where

import OthelloTools

------------------------Running FSM on a board-----------------------------

--validMovesOnBoard :: Board -> [(Int, Int)]
-- can't write this yet since we will need the unrotate function

findMoves :: Board -> [(Int, Int)]
findMoves [] = []
findMoves x = map head (findMovesAndCaptures' x)

findMovesAndCaptures :: Board -> Cell -> [[(Int, Int)]]
findMovesAndCaptures b cell
  | cell == W = findMovesAndCaptures' (invertBoardPieces b)
  | otherwise = findMovesAndCaptures' b

findMovesAndCaptures' :: Board -> [[(Int, Int)]]
findMovesAndCaptures' [] = []
findMovesAndCaptures' (x:xs) = movesAndCapturesOnRow (succ (length xs)) (runFSML x) ++ findMovesAndCaptures' xs

--This might be usefull in speeding things up if you make it without making calls to movesandcaptures
validMovesOnRow :: [[(Int, Int)]] -> [(Int,Int)]
validMovesOnRow [] = []
validMovesOnRow (x:xs) = head x : validMovesOnRow xs

movesAndCapturesOnRow :: Int -> [(Int, Int, Int)] -> [[(Int, Int)]]
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
{- takes a board and returns the board with every piece flipped -}
invertBoardPieces :: Board -> Board
invertBoardPieces [] = []
invertBoardPieces (x:xs) = (invertRowPieces x) : (invertBoardPieces xs)

-- | invertRowPieces
{- subrutine of invertBoardPieces -}
invertRowPieces :: [Cell] -> [Cell]
invertRowPieces [] = []
invertRowPieces (x:xs) = (otherCell x) : (invertRowPieces xs)

----------------------------------FSM--------------------------------------

type State = (Int, (Int, Int), Int, (Int,Int), [(Int, Int, Int)])

-- | fsm
fsm :: Cell -> State -> State
fsm E (-1, _, _, _, _) = fsmInit E
fsm B (-1, _, _, _, _) = fsmInit B
fsm W (-1, _, _, _, _) = fsmInit W
fsm E s = fsmLeftCalc E (fsmRightCalc E s)
fsm B s = fsmLeftCalc B (fsmRightCalc B s)
fsm W s = fsmLeftCalc W (fsmRightCalc W s)

fsmInit :: Cell -> State
fsmInit E = (1, (1, 0), 0, (1, 0), [])
fsmInit B = (0, (1, 0), 1, (1, 0), [])
fsmInit W = (0, (1, 0), 0, (1, 0), [])

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

fsmMemManage :: State -> [(Int, Int, Int)]
fsmMemManage (x, y, z, a, mem) = compress mem
    where compress :: [(Int, Int, Int)] -> [(Int, Int, Int)]
          compress [] = []
          compress (x:[]) = (x:[])
          compress (x:xs:xss)
            | (first x) == (first xs) = (x `merge` xs) : compress xss
            | otherwise = (x : (compress (xs:xss)))

merge :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
merge (x1, x2, x3) (y1, y2, y3)
    | x2 == 0 = (x1, y2, y3)
    | otherwise = (x1, x2, y3)

first :: (Int, Int, Int) -> Int
first (f, _, _) = f

runFSM :: [Cell] -> [(Int, Int, Int)]
runFSM row = fsmMemManage (foldr fsm (-1, (0,0), -1, (0,0), []) row)


--------FSML
fsml :: State -> Cell -> State
fsml x y = fsm y x

runFSML  :: [Cell] -> [(Int, Int, Int)]
runFSML row = fsmMemManage (foldl fsml (-1, (0,0), -1, (0,0), []) row)

