import Game
import Data.List

main = do
  fileContents <- readFile "board.txt"
  putStrLn "Digite a quantidade de iteracoes:"
  numIterations <- getLine
  let
    startBoard = boardFromString fileContents
    evolutions = take (read numIterations) (iterate evolve startBoard)
    boards = removeDuplicates evolutions
  printGame boards
  printResult boards evolutions