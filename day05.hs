module Day05 where

testInput :: [Int]
testInput = [0, 3, 0, 1, -3] -- but in a newline delimited string

expectedResult :: Int
expectedResult = 5 -- 5 steps

data Maze = Maze {index        :: Int
                 ,instructions :: [Int]}

mazeInput :: Maze
mazeInput= Maze 0 testInput

nextStep :: Maze -> Maze
nextStep (Maze index instructions)
  | mazeFinished (Maze index instructions) = Maze index instructions -- do nothing
  | otherwise = Maze nextIndex nextInstructions
    where
      indexVal = instructions !! index
      nextIndex = indexVal + index
      prefix = take index instructions
      suffix = drop (index + 1) instructions
      nextInstructions = prefix ++ [indexVal + 1] ++ suffix

mazeFinished (Maze index instructions) = index < 0 || index >= length instructions

numStepsTillCompletion maze
  | mazeFinished maze = 0
  | otherwise = 1 + (numStepsTillCompletion . nextStep) maze

numToCompletion :: Integer
numToCompletion = numStepsTillCompletion mazeInput

parseToMaze :: String -> Maze
parseToMaze str =
  Maze 0 (map read (lines str))


main :: IO ()
main =
  do
    testInput <- readFile "day05/input.txt"
    print . numStepsTillCompletion . parseToMaze $ testInput
