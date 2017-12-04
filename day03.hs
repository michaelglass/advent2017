module Day03 where

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
