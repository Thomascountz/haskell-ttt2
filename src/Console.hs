module Console (
   boardStr,
   prompt
   ) where
import Board

boardStr :: Board -> String
boardStr board = stringifyBoard board 0 ""

stringifyBoard :: Board -> Int -> String -> String
stringifyBoard board index str
  | index == length board = str ++ gutter board
  | index `mod` size board == 0 = stringifyBoard board (index + 1) (str ++ gutter board ++ "| " ++ symbolCellStr board index)
  | otherwise = stringifyBoard board (index + 1) (str ++ symbolCellStr board index)
  

symbolCellStr :: Board -> Int -> String
symbolCellStr board index = case board !! index of
  Empty -> buffer index ++ " | "
  X -> "XX | "
  O -> "OO | "


buffer :: Int -> String
buffer int 
  | int < 10 = "0" ++ show int
  | otherwise = show int
  
gutter :: Board -> String
gutter board = "\n" ++ replicate ((size board * 5) + 1) '-' ++ "\n"

prompt :: Board -> String
prompt board = "Please enter a coordinate [0-" ++ show (length board - 1) ++ "]"