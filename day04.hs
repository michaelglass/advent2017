module Day04 where

import           Data.Set (Set)
import qualified Data.Set as Set

passphrasePath = "day04/passphrases.txt"

numValidPassphrases input =
  length (filter validPassword (lines input))

numValidPassphrasesWithoutAnagrams input =
  length (filter validPasswordWithoutAnagram(lines input))

validPassword :: String -> Bool
validPassword str =
  numWords == numUniqueWords
  where
    allWords = words str
    numWords = length allWords
    numUniqueWords = Set.size . Set.fromList $ allWords

validPasswordWithoutAnagram :: String -> Bool
validPasswordWithoutAnagram str =
  numWords == numUniqueLetSets
  where
    allWords = words str
    numWords = length allWords
    allLetterSets = map Set.fromList allWords
    numUniqueLetSets = Set.size . Set.fromList $ allLetterSets

tests = [("aa bb cc dd ee", True)
        ,("aa bb cc dd aa", False)
        ,("aa bb cc dd aaa", True)]

testInputs = map fst tests
expectedResults = map snd tests
testResults = map validPassword testInputs

main :: IO ()
main =
  do
    putStrLn "Running tests:"
    if testResults == expectedResults then
      do
        putStrLn "tests pass, running over real input\n Num valid passphrases:"
        passphrases <- readFile passphrasePath
        print . numValidPassphrases $ passphrases
        putStrLn "constraining without anagrams"
        print . numValidPassphrasesWithoutAnagrams $ passphrases
    else
      do
        putStrLn "tests fail"
        print testResults
