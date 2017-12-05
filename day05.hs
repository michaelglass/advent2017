module Day05 where

testInput :: [Int]
testInput = [0, 3, 0, 1, -3] -- but in a newline delimited string

expectedResult :: Integer
expectedResult = 5 -- 5 steps

data Zipper a = Zipper{ before::[a]
                      , currentAndAfter::[a]} deriving (Show, Eq)

zipperInput :: Zipper Int
zipperInput = Zipper [] testInput

nextStep :: Zipper Int -> Zipper Int
nextStep (Zipper prev []) = Zipper prev []
nextStep (Zipper [] next) = Zipper [] next
nextStep (Zipper prev (c:next)) =
  step c (Zipper prev (c+1:next))
  where
    -- moving right (+1)
    -- [1,2,3] [4,5,6]  => [1,2,3,4] [5,6]
    -- (bef ++ take 1 aft) (drop 1 aft)
    -- moving left (-1)
    -- [1,2,3] [4,5,6]  => [1,2] [3,4,5,6]
    -- (take (len - 1) bef) (drop (len - 1) bef ++ aft)
    step 0 zipper = zipper
    step shift (Zipper bef aft)
      | shift > 0 = Zipper (bef ++ take shift aft) (drop shift aft)
      | otherwise = Zipper (take negShift bef) (drop negShift bef ++ aft)
        where
          negShift = length bef + shift

numStepsTillCompletion :: Num t => Zipper Int -> t
numStepsTillCompletion zipper =
  numStepsTillCompletion' 0 zipper
  where
    numStepsTillCompletion' acc (Zipper _ []) = acc
    numStepsTillCompletion' acc (Zipper [] _) = acc
    numStepsTillCompletiono' acc zipper = numStepsTillCompletion' (acc + 1) (nextStep zipper)

numToCompletion :: Integer
numToCompletion = numStepsTillCompletion zipperInput

parseToZipper :: String -> Zipper Int
parseToZipper str =
  Zipper [] (map read (lines str))


main :: IO ()
main =
  do
    testInput <- readFile "day05/input.txt"
    print . numStepsTillCompletion . parseToZipper $ testInput
