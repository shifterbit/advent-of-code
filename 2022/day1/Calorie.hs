{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO as IO

type Elf = [Int]

-- | Finds the elf with the highest calorie count
highestCalorieCount :: [Elf] -> Int
highestCalorieCount [] = 0
highestCalorieCount xs = foldr (max . sum) 0 xs

-- | Get the three Elves with the highest calorie counts
topThreeCalorieCounts :: [Elf] -> [Int]
topThreeCalorieCounts xs = (take 3 . reverse . sort) (map sum xs)

parseInput :: T.Text -> [Elf]
parseInput str = result
  where
    counts = T.splitOn "\n\n" str
    lists = map (lines . T.unpack) counts
    result = map (fmap read) lists

-- | Part 1 Solution:
--   Reads Input from a file and Finds the Elf with the highest calorie count
mostCalories :: FilePath -> IO Int
mostCalories path = do
  contents <- TIO.readFile path
  return $ (highestCalorieCount . parseInput) contents

-- | Part 2 Solution
--   Reads Input from a file and Finds the three Elves with the highest calorie counts
topThreeCalorieCountsCombined :: FilePath -> IO Int
topThreeCalorieCountsCombined path = do
  contents <- TIO.readFile path
  return $ (sum . topThreeCalorieCounts . parseInput) contents
