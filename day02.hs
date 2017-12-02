module Day02 where
import           Data.Char

testFile1 = "day02/test1.txt"
testFile2 = "day02/test2.txt"
testResult1 = 18
testResult2 = 9
realFile = "day02/real.txt"

checksum :: (Show a, Num a, Ord a) => [[a]] -> a
checksum input = sum $ map lineDifferences input

lineDifferences :: (Ord a, Foldable t, Num a) => t a -> a
lineDifferences line = maximum line - minimum line

checksum2 :: Integral a => [[a]] -> a
checksum2 input = sum $ map howManyMultiples input


howManyMultiples :: Integral a => [a] -> a
howManyMultiples line = larger `div` smaller
  where
    (smaller, larger) = head (allMultiples line)
    multiples = allMultiples line

allMultiples :: Integral t => [t] -> [(t, t)]
allMultiples line =
  [(i,j) |
    i <- line,
    j <- line ,
    i < j && j `mod` i == 0]


inputToNums :: String -> [[Int]]
inputToNums input = map lineToInts (lines input)
  where
    lineToInts :: String -> [Int]
    lineToInts input = map read (words input)

main :: IO ()
main =
  do
    testInput1 <- readFile testFile1
    realInput <- readFile realFile
    putStrLn "step 1:"
    printResults checksum testInput1 testResult1 realInput
    putStrLn "step 2:"
    testInput2 <- readFile testFile2
    printResults checksum2 testInput2 testResult2 realInput

printResults :: (Show a, Eq a) => ([[Int]] -> a) -> String -> a -> String -> IO ()
printResults fn testInput testResult realInput =
    if testChecksum == testResult then
      do
        putStrLn "works, checksum is"
        print realChecksum
    else
      do
        print testChecksum
        putStrLn "is wrong, should be:"
        print testResult
    where
      testChecksum = fn . inputToNums $ testInput
      realChecksum = fn . inputToNums $ realInput
