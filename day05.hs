module Day05 where

testInput :: [Int]
testInput = [0, 3, 0, 1, -3] -- but in a newline delimited string

expectedResult :: Int
expectedResult = 5 -- 5 steps

data Maze = Maze {index        :: Int
                 ,instructions :: [Int]}

mazeInput :: Maze
mazeInput= Maze 0 testInput

nextStep :: (Int -> Int) -> Maze -> Maze
nextStep offset (Maze index instructions)
  | mazeFinished (Maze index instructions) = Maze index instructions -- do nothing
  | otherwise = Maze nextIndex nextInstructions
    where
      indexVal = instructions !! index
      nextIndex = indexVal + index
      prefix = take index instructions
      suffix = drop (index + 1) instructions
      nextInstructions = prefix ++ [indexVal + (offset indexVal)] ++ suffix

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
  Maze 0 (map read (lines str))

main :: IO ()
main =
  do
    testInput <- readFile "day05/input.txt"
    print . numToCompletion1 . parseToMaze $ testInput
    print . numToCompletion2 . parseToMaze $ testInput
