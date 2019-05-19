module Top10WordsInFile where

import System.IO
import Data.List    --sortOn
import qualified Data.Map as Map

mainTop10 = do
          input <- readFile "bible+shakes.nopunc"
          putStrLn "Top 10 the most popular words in 'bible+shakes.nopunc' file:\n"
          let top10 = getTop10 input
          print top10
          let numOfLinesWithoutTop10 = countLinesWithoutTop10 top10 input
          print numOfLinesWithoutTop10

getTop10 :: String -> [(String, Int)]
getTop10 input = take 10 . reverse $ sortOn snd (Map.toList ( mapWords (words input) Map.empty ))

mapWords:: [String] -> Map.Map String Int -> Map.Map String Int
mapWords [] map = map
mapWords wrds map = mapWords (tail wrds) (Map.insertWith (+) (head wrds) 1 map)

countLinesWithoutTop10 :: [(String, Int)] -> String -> Int
countLinesWithoutTop10 top10WordsTuples input = sum [1 | line <- lines input, null (words line `intersect` popular)]
                        where popular = map fst top10WordsTuples