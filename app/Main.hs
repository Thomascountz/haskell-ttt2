module Main where
import Board
import Minimax
import Console

main :: IO ()
main = do
  putStrLn "(~O_o)~ Tic Tac Toe ~(o_O~)"
  let board = initBoard 3
  play board

play :: Board -> IO ()
play board = do
  putStrLn (boardStr board)
  putStrLn (prompt board)
