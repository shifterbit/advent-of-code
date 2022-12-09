import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO as IO

type Elf = [Int]

highestCalorieCount :: [Elf] -> Int
highestCalorieCount [] = 0
highestCalorieCount xs = foldr (max . sum) 0 xs

topThreeCalorieCounts :: [Elf] -> [Int]
topThreeCalorieCounts xs = (take 3 . reverse . sort) (map sum xs)

parseInput :: T.Text -> [[Int]]
parseInput str = result
  where
    counts = T.splitOn (T.pack "\n\n") str
    lists = map (lines . T.unpack) counts
    result = map (fmap read) lists


-- Part 1 Solution
mostCalories :: FilePath -> IO Int
mostCalories path = do
  contents <- TIO.readFile path
  return $ (highestCalorieCount . parseInput) contents

-- Part 2 Solution
topThreeCalorieCountsCombined :: FilePath -> IO Int
topThreeCalorieCountsCombined path = do
  contents <- TIO.readFile path
  return $ (sum . topThreeCalorieCounts . parseInput) contents
