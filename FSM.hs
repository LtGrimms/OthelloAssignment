module FSM where

import OthelloTools

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
          compress (x:xs:xss)
            | (first x) == (first xs) = (x `merge` xs) : compress xss
            | otherwise = (x : xs : (compress xss))

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

--------MemoryExtraction

extractMoves :: State -> [Int]
extractMoves (_, _, _, _, []) = []
extractMoves (a, b, c, d, x:xs) = moveFromMem x : extractMoves (a, b, c, d, xs)

moveFromMem :: (Int, Int, Int) -> Int
moveFromMem (col, _, _) = col