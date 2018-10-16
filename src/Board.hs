module Board (
  Symbol (..),
  Board,
  initBoard,
  size,
  result,
  tie,
  win
) where

data Symbol = X | O | Empty deriving (Eq, Show)
type Board = [Symbol]

initBoard :: Int -> Board
initBoard size = replicate (size^2) Empty

size :: Board -> Int
size = round . sqrt . fromIntegral . length

result :: Board -> Int -> Symbol -> Board
result [] _ _ = []
result (head:tail) index symbol 
  | index == 0 = symbol:tail
  | otherwise = head:result tail (index - 1) symbol

tie :: Board -> Bool
tie = notElem Empty

win :: Board -> Bool
win board = any (winAt board) (winningIndices board)

winAt :: Board -> [Int] -> Bool
winAt board indices = all (== X) (board `atIndices` indices) ||
                        all (== O) (board `atIndices` indices)

atIndices :: Board -> [Int] -> [Symbol]
atIndices board = map (\i -> board !! i)

winningIndices :: Board -> [[Int]]
winningIndices board = winningRowIndices board ++ winningColIndices board ++ winningDiagIndices board

winningRowIndices :: Board -> [[Int]]
winningRowIndices board = map (\start -> take (size board) [start..]) (rowStarts board)

winningColIndices :: Board -> [[Int]]
winningColIndices board = map (\start -> take (size board) [start, start + size board..]) (colStarts board)

winningDiagIndices :: Board -> [[Int]]
winningDiagIndices board = [
                      take (size board) [0, (size board + 1)..],
                      take (size board) [size board - 1, ((size board - 1) ^ 2)..]
                     ]

rowStarts :: Board -> [Int]
rowStarts board = take (size board) [0, (0 + size board)..]

colStarts :: Board -> [Int]
colStarts board = take (size board) [0..]

