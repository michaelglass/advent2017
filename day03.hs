module Day03 where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

-- 65 64 63 62 61 60 59 58 57
-- 66 37 36 35 34 33 32 31 56
-- 67 38 17 16 15 14 13 30 55
-- 68 39 18 5  4  3  12 29 54
-- 69 40 19 6  1  2  11 28 53
-- 70 41 20 7  8  9  10 27 52
-- 71 42 21 22 23 24 25 26 51
-- 72 43 44 45 46 47 48 49 50
-- 73 74 75 76 77 78 79 80 81

--- another view (by coordinates)
-- {1(0,0)}, {2(1,0)}, {3,(1,1)}, {4,(0,1)}, {5,(-1,1)), {6,(-1,0)}, {7,(-1,-1)}, {8,(0,-1)}, {9(1,-1)}, {10(2,-1)}...
-- another view (by ring)
-- [1, [2..9], [10..25], [26..49], [50..81], [82..121]]
-- test num: 11
-- last odd square? sqrt(11) = 3.3, 3
-- distance between 11 - (3^3) = 2
-- 2 `divMod`  (3+1) = (0,2) -- 2 is distance from previous step, 3 is prev square
-- e.g. 0 mean
-- if first digit =
  -- 0 -> right, ( 2, _)
  -- 1 -> top    ( _, 2)
  -- 2 -> left   (-2, _)
  -- 3 -> bottom ( _,-2)
-- snd digit means distance from corner
  -- right  -> (   2,(1-2)+_)
  -- top    -> ( 2-_,      2)
  -- left   -> (  -2,    2-_)
  -- bottom -> (-2+_,     -2)


input = 289326

lastOddSquare :: Integer -> Integer
lastOddSquare x =
  if even lastSquare then
    lastSquare - 1
  else
    lastSquare
  where
  lastSquare = floor . sqrt . fromIntegral $ x


-- spiralPosition :: (Integral t, RealFrac t, Floating t) => t -> (t, t)
spiralPosition :: Integer -> (Integer, Integer)
spiralPosition x =
  if side == 0 && offset == 0 then
    (distanceToOrigin - 1, negate (distanceToOrigin - 1))
  else
    case side of
      0 -> (distanceToOrigin, offset - distanceToOrigin) -- right
      1 -> (distanceToOrigin - offset, distanceToOrigin)
      2 -> (negate distanceToOrigin, distanceToOrigin - offset)
      3 -> (negate distanceToOrigin + offset, negate distanceToOrigin)
      _ -> (-1, -1)
  where
    lastOddSq = lastOddSquare x
    distanceToSq = x - lastOddSq * lastOddSq
    sideLength = lastOddSq + 1
    (side, offset) = divMod distanceToSq sideLength
    distanceToOrigin = (lastOddSq + 1) `div` 2

distanceToOrigin :: Integer -> Integer
distanceToOrigin x =
  abs a + abs b
  where
    (a,b) = spiralPosition x

part1Answer = spiralPosition input

-- now we have to find the first value greater than our input
-- where each part of the spiral is a sum of previous neigbors
-- e.g.
--  5  4  2  57
-- 10  1  1  54
-- 11  23 25 26
data SumPoint = SumPoint { val :: Integer
                     , summed:: Integer
                     , loc :: (Integer,Integer) } deriving (Show, Eq, Ord)

nextSumSpiral :: [SumPoint] -> [SumPoint]
nextSumSpiral [] = [sumPoint]
  where
    sumPoint = SumPoint 1 1 (0,0)

nextSumSpiral (x:xs) = nextPoint : x : xs
  where
      spiral = x:xs
      nextVal = val x + 1
      nextPosition = spiralPosition nextVal
      nextSum = sumOf nextPosition spiral
      nextPoint = SumPoint nextVal nextSum nextPosition

-- expectation: (x,y) not in spiralArr
sumOf :: (Integer, Integer) -> [SumPoint] -> Integer
sumOf (x,y) spiralArr = sum vals
  where
    tuples = map (\i -> (loc i, i) ) spiralArr
    spiralMap = Map.fromList tuples
    neighbors = catMaybes [Map.lookup (a,b) spiralMap | a <- [-1+x..1+x], b <- [-1+y..1+y]]
    vals = map summed neighbors

sumUntil :: [SumPoint] -> [SumPoint]
sumUntil spiral
  | spiral == [] || ((summed . head) spiral < input) = (sumUntil . nextSumSpiral) spiral
  | otherwise = spiral

part2Answer = summed . head . sumUntil $ []
