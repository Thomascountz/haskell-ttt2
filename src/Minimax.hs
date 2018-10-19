module Minimax where
import Board
import Data.List


minimax' :: Board -> [(Int, Int)]
minimax' board = (map (\move -> (move, minPlay' (result board move O) (availableSpaces (result board move O)) 0 (-1000) 1000)) (availableSpaces board))

minPlay' :: Board -> [Int] -> Int -> Int -> Int -> Int
minPlay' board spaces depth α β
  | terminal board = utility' board depth
  | null spaces = β
  | otherwise =
      if α >= β' then β' else minPlay' board (tail spaces) depth α β'
      where β' = min (maxPlay' (result board (head spaces) X) (tail spaces) (depth + 1) α β) β

maxPlay' :: Board -> [Int] -> Int -> Int -> Int -> Int
maxPlay' board spaces depth α β
  | terminal board = utility' board depth
  | null spaces = α
  | otherwise =
       if α'>= β then α' else maxPlay' board (tail spaces) depth α' β
       where α' = max (minPlay' (result board (head spaces) O) (tail spaces) (depth + 1) α β) α

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

utility' :: Board -> Int -> Int
utility' board depth
  | tie board = 0
  | win board && player board == O = -100 + depth
  | win board && player board == X = 100 - depth

utility :: Board -> Int
utility board
  | tie board = 0
  | win board && player board == O = -1
  | win board && player board == X = 1
