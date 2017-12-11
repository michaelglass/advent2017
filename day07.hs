module Day07 where

import qualified Data.Map  as Map
import           Data.Tree


testInput = "pbga (66)\n\
            \xhth (57)\n\
            \ebii (61)\n\
            \havc (66)\n\
            \ktlj (57)\n\
            \fwft (72) -> ktlj, cntj, xhth\n\
            \qoyq (66)\n\
            \padx (45) -> pbga, havc, qoyq\n\
            \tknk (41) -> ugml, padx, fwft\n\
            \jptl (61)\n\
            \ugml (68) -> gyxo, ebii, jptl\n\
            \gyxo (61)\n\
            \cntj (57)"

parseLine :: String -> ((String, Integer), [String])
parseLine input = ((name, weight), children)
  where
    (name:weightStr:rest) = words input
    weight = read weightStr
    children = map keepAlpha (drop 1 rest)
    keepAlpha = filter (\a -> elem a ['a'..'z'])

parseTree :: String -> Map.Map String String
parseTree input = Map.fromList mapified
  where
    allLines = map parseLine $ lines input
    mapified = concatMap mapifyInput allLines

mapifyInput :: ((String, Integer), [String]) -> [(String, String)]
mapifyInput ((parent, _), children) = map (\x -> (x, parent)) children

-- findParent :: Map.Map String String -> String
findParent :: Map.Map String String -> String
findParent input = findParent' firstTest input
  where
    firstTest = snd.head.Map.toList $ input
    findParent' word tree =
      if Map.member word tree then
        findParent' ((Map.!) tree word) tree
      else
        word

testThatItWorkds = findParent . parseTree $ testInput
itWorked = testThatItWorkds == "tknk"

main :: IO ()
main =
  do
    testInput <- readFile "day07input.txt"
    putStrLn "The root is"
    print (findParent . parseTree $ testInput)
