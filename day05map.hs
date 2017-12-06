module Day05 where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap


testInput :: [Int]
testInput = [0, 3, 0, 1, -3] -- but in a newline delimited string

expectedResult :: Int
expectedResult = 5 -- 5 steps

data Maze = Maze {index        :: Int
                 ,instructions :: IntMap Int}

mazeInput :: Maze
mazeInput= Maze 0 (IntMap.fromList (zip [0..] testInput))

nextStep :: (Int -> Int) -> Maze -> Maze
nextStep offset (Maze index instructions)
  | mazeFinished (Maze index instructions) = Maze index instructions -- do nothing
  | otherwise = Maze nextIndex nextInstructions
    where
      indexVal = instructions IntMap.! index
      nextIndex = indexVal + index
      nextInstructions = IntMap.insert index (indexVal + (offset indexVal)) instructions

mazeFinished (Maze index instructions) = index < 0 || index >= length instructions

numStepsTillCompletion offset maze
  | mazeFinished maze = 0
  | otherwise = 1 + numStepsTillCompletion offset (nextStep offset maze)

step1Offset _ = 1

step2Offset x
  | x >= 3 = -1
  | otherwise = 1

numToCompletion1 = numStepsTillCompletion step1Offset
numToCompletion2 = numStepsTillCompletion step2Offset

parseToMaze :: String -> Maze
parseToMaze str =
  Maze 0 (IntMap.fromList indexedNums)
  where
    nums = map read (lines str)
    indexedNums = zip [0..] nums

main :: IO ()
main =
  do
    testInput <- readFile "day05/input.txt"
    print . numToCompletion1 . parseToMaze $ testInput
    print . numToCompletion2 . parseToMaze $ testInput
