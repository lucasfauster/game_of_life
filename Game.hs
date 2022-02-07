module Game where
import Data.List (intersect)
import Data.Foldable (sequenceA_)
import Data.Array

type Board = Array Coordinate Cell
type Coordinate = (Int, Int)
type Boundary   = (Coordinate, Coordinate)
data Cell       = Alive | Dead | Zombie
    deriving Eq

instance Show Cell where
  show Alive = "*"
  show Dead  = "."
  show Zombie = "o"


printGame :: [Board] -> IO()
printGame = mapM_ printGrid

printGrid :: Show a => Array (Int, Int) a -> IO [()]
printGrid grid = mapM (putStrLn . textRepresentation) (toSimpleArray grid ++ [[]])

toSimpleArray :: Array (Int, Int) a -> [[a]]
toSimpleArray grid = [[grid ! (x, y) | y<-[lowy..highy]] |  x<-[lowx..highx]]
  where ((lowx, lowy), (highx, highy)) =  bounds grid

textRepresentation :: Show a => [a] -> String
textRepresentation = foldl (\acc y -> acc ++ show y ++ " ") ""

printResult :: [a] -> [a] -> IO()
printResult b e
  | length b /= length e = putStrLn ("Evoluções para estabilizar: " ++ show (length b))
  | otherwise = putStrLn ("Sequência das " ++ show (length e) ++ " primeiras evoluções")

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates list = remDups list []

remDups :: (Eq a) => [a] -> [a] -> [a]
remDups [] _ = []
remDups (x:xs) list2
    | x `elem` list2 = remDups xs list2
    | otherwise = x : remDups xs (x:list2)

evolve :: Board -> Board
evolve board = listArray (bounds board) boardValues
  where
    boardCoordinates   = indices board
    boardWithNeighbors = map (map (board !) . intersect boardCoordinates . neighbors) boardCoordinates
    cellsAndNeighbors  = zip (elems board) boardWithNeighbors
    boardValues        = map evolveCell cellsAndNeighbors

neighbors :: Coordinate -> [Coordinate]
neighbors (x, y) = [

  (x-1,y-1), (x,y-1), (x+1,y-1),
  (x-1,y  ),          (x+1,y),
  (x-1,y+1), (x,y+1), (x+1,y+1)]

isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead = False
isAlive Zombie = False

isZombie :: Cell -> Bool
isZombie Alive = False
isZombie Dead = False
isZombie Zombie = True

evolveCell :: (Cell, [Cell]) -> Cell
evolveCell (Alive, neighbors)
  | countZombies neighbors >= 2 = Zombie
  | countLiving neighbors < 2 && countZombies neighbors < 2 = Dead
  | countLiving neighbors > 3 && countZombies neighbors == 0 = Dead
  | otherwise                    = Alive

evolveCell (Dead, neighbors)
  | countLiving neighbors == 3 = Alive
  | otherwise                    = Dead

evolveCell (Zombie, neighbors)
  | countLiving neighbors == 0 = Dead
  | otherwise                    = Zombie

countLiving = length . filter isAlive

countZombies = length . filter isZombie

boardFromString :: String -> Board
boardFromString input = listArray boardBounds boardCells
  where
    boardRows = inputStringToBoardRows input
    boardBounds = boardRowsToBoardBounds boardRows
    boardCells = boardRowsToCells boardRows

inputStringToBoardRows :: String -> [[Char]]
inputStringToBoardRows input = removeBlankLines (lines input)

removeBlankLines :: [[Char]] -> [[Char]]
removeBlankLines = filter (/="")

boardRowsToBoardBounds :: [[Char]] -> Boundary
boardRowsToBoardBounds boardRows = ((0, 0), (xBoundary, yBoundary))
  where
    xBoundary = length boardRows - 1
    yBoundary = foldl (\max row -> if length row > max then length row else max ) 0 boardRows - 1

boardRowsToCells :: [[Char]] -> [Cell]
boardRowsToCells boardRows = [charToCell x | x <- concat boardRows]

charToCell :: Char -> Cell
charToCell '*' = Alive
charToCell '.' = Dead
charToCell 'o' = Zombie
charToCell _   = error "Célula inválida"