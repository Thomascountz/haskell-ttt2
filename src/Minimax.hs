module Minimax where
import Board
import Data.List


minimax' :: Board -> Int
minimax' board = fst $ maximumBy (\(_, a) (_, b) -> compare a b) (map (\move -> 
  (
    move, 
    minPlay' (result board move O) (availableSpaces (result board move O)) (-1000) 1000)
  )
  (availableSpaces board))

minimax'' :: Board -> [(Int, Int)]
minimax'' board = map (\move -> 
    (
      move, 
      minPlay' (result board move O) (availableSpaces (result board move O)) (-2) 2
    )
  )
  (availableSpaces board)

minPlay' :: Board -> [Int] -> Int -> Int -> Int
minPlay' board spaces α β
  | terminal board = utility' board
  | null spaces = β
  | otherwise =
      if α >= β' then β' else minPlay' board (tail spaces) α β'
      where β' = min (maxPlay' (result board (head spaces) X) (tail spaces) α β) β

maxPlay' :: Board -> [Int] -> Int -> Int -> Int
maxPlay' board spaces α β
  | terminal board = utility' board
  | null spaces = α
  | otherwise =
       if α'>= β then α' else maxPlay' board (tail spaces) α' β
       where α' = max (minPlay' (result board (head spaces) O) (tail spaces) α β) α

player :: Board -> Symbol
player board = if odd (length board) == odd (length (filter (==Empty) board)) then X else O

availableSpaces :: Board -> [Int]
availableSpaces = elemIndices Empty

terminal :: Board -> Bool
terminal board = tie board || win board

utility' :: Board -> Int
utility' board
  | tie board = 0
  | win board && player board == O = -1
  | win board && player board == X = 1

