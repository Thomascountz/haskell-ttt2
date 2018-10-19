module Main where
import Board
import Minimax
import Console
import Text.Read
import Data.List

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
  else
    case player board of 
      O -> do
          let position = minimax' board
          print position
          let board' = result board position O
          play board'
      X -> do
          position <- getPosition board
          let board' = result board position X
          play board'

getPosition :: Board -> IO Int
getPosition board = do
    putStrLn (promptMessage board)
    input <- getLine
    let position = readMaybe input :: Maybe Int
    case position of
      Just n -> if spaceIsAvailable board n
                then return n
                else do
                  putStrLn (spaceUnavailableMessage n)
                  getPosition board
      Nothing -> do
        putStrLn (invalidInputMessage input)
        getPosition board