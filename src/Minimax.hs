module Minimax where
import Board
import Data.List

minimax' :: Board -> Int
minimax' board = fst (maximumBy (\(_, a) (_, b) -> compare a b) (map (\move -> (move, minPlay (result board move O))) (availableSpaces board)))

minPlay' :: Board -> Int
minPlay' board = if terminal board
                then utility board
                else minimum (
                  score = playMoveMax board (head (availableSpaces board))
                )

playMoveMax :: Board -> Int -> Int
playMoveMax board position = minPlay (result board position X)

playMoveMin :: Board -> Int -> Int
playMoveMin board position = maxPlay (result board position O)

maxPlay' :: Board -> Int
maxPlay' board = if terminal board
                then utility board
                else maximum (map (\move -> minPlay (result board move O)) (availableSpaces board))


minimax :: Board -> Int
minimax board = fst (maximumBy (\(_, a) (_, b) -> compare a b) (map (\move -> (move, minPlay (result board move O))) (availableSpaces board)))

minPlay :: Board -> Int
minPlay board = if terminal board
                then utility board
                else minimum (map (\move -> maxPlay (result board move X)) (availableSpaces board))

maxPlay :: Board -> Int
maxPlay board = if terminal board
                then utility board
                else maximum (map (\move -> minPlay (result board move O)) (availableSpaces board))

player :: Board -> Symbol
player board = if odd (length board) == odd (length (filter (==Empty) board)) then X else O

availableSpaces :: Board -> [Int]
availableSpaces = elemIndices Empty

terminal :: Board -> Bool
terminal board = tie board || win board

utility :: Board -> Int
utility board
  | tie board = 0
  | win board && player board == O = -1
  | win board && player board == X = 1
