module Console 
  (boardStr) 
where
import Board

boardStr :: Board -> Int -> String -> String
boardStr board index str
  | index == length board = str ++ gutter board
  | index `mod` size board == 0 = boardStr board (index + 1) (str ++ gutter board ++ "| " ++ symbolCellStr board index)
  | otherwise = boardStr board (index + 1) (str ++ symbolCellStr board index)
  

symbolCellStr :: Board -> Int -> String
symbolCellStr board index = case board !! index of
  Empty -> buffer index ++ " | "
  X -> "XX" ++ " | "
  O -> "OO" ++ " | "


buffer :: Int -> String
buffer int 
  | int < 10 = "0" ++ show int
  | otherwise = show int
  
gutter :: Board -> String
gutter board = "\n" ++ replicate ((size board * 5) + 1) '-' ++ "\n"