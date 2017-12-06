module Day06 where

import           Data.Foldable
import           Data.List
import qualified Data.Sequence as Seq

redistribute blocks =
  redistribute' nextIndex largestVal blocksWithMaxZeroed
  where
    indexOfLargest =
      head (elemIndices largestVal blocks)
    largestVal = maximum blocks
    blocksWithMaxZeroed = toList (Seq.update indexOfLargest 0 (Seq.fromList blocks))
    nextIndex = if indexOfLargest + 1 == length blocks then 0 else indexOfLargest + 1
    -- redistribute' :: Int ind val blocks =
    redistribute' _ 0 blocks = blocks
    redistribute' ind toRedistribute blocks =
      redistribute' nextInd (toRedistribute - 1) blocksWithAddition
      where
        nextInd = if ind + 1 == length blocks then 0 else ind + 1
        blocksWithAddition = toList (Seq.update ind (valAtIndex + 1) (Seq.fromList blocks))
        valAtIndex = blocks !! ind

redistributeUntilLoop blocks =
  redistributeUntilLoop' [] blocks
  where
    redistributeUntilLoop' history blocks
      | elem blocks history = blocks : history
      | otherwise = redistributeUntilLoop' (blocks : history) (redistribute blocks)

realInput = [10, 3,	15, 10, 5, 15, 5, 15, 9, 2, 5, 8, 5, 2, 3, 6]
testInput = [0, 2,7,0]
main =
  do
    print cycles
    print (cycles - repeatedInd)
  where
    (repeated : history) = redistributeUntilLoop realInput
    cycles = length history
    repeatedInd = head (elemIndices repeated (reverse history))
