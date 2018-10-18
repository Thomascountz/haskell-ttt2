module Main where
import Board
import Minimax
import Console
import Text.Read

main :: IO ()
main = do
  putStrLn "(~O_o)~ Tic Tac Toe ~(o_O~)"
  let board = initBoard 3
  play board

play :: Board -> IO ()
play board = do
  putStrLn (boardStr board)
  if terminal board
  then putStrLn (endOfGameMessage board)
  else do
    putStrLn (promptMessage board)
    case player board of 
      O -> do
          let position = minimax board
          print position
          let board' = result board position O
          play board'
      X -> do
          input <- getLine
          let position = read input :: Int
          let board' = result board position X
          play board'
