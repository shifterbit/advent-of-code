{-# LANGUAGE OverloadedStrings #-}

import Data.List (intersect)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

newtype SectionAssignment = Assignment [Int] deriving (Show)

type AssignmentPair = (SectionAssignment, SectionAssignment)

sampleInput =
  T.unlines
    [ "2-4,6-8",
      "2-3,4-5",
      "5-7,7-9",
      "2-8,3-7",
      "6-6,4-6",
      "2-6,4-8"
    ]

-- | Gets the first two elements from a list as a pair
pairFromList :: [a] -> (a, a)
pairFromList (a : b : xs) = (a, b)
pairFromList a = (head a, head a)

rangeFromPair :: Enum a => (a, a) -> [a]
rangeFromPair (a, b) = [a .. b]

unpackTuple :: (T.Text, T.Text) -> (String, String)
unpackTuple (a, b) = (T.unpack a, T.unpack b)

readTuple :: (Read a) => (String, String) -> (a, a)
readTuple (a, b) = (read a, read b)

-- | Parses Text as Assignment Pairs
--   [ "2-4,6-8",
--   "2-3,4-5"
--   ]
--   becomes:
--   [ (Assignment [2,3,4],Assignment [6,7,8]),
--     (Assignment [2,3],Assignment [4,5])
--   ]
parseInput :: T.Text -> [AssignmentPair]
parseInput text = result
  where
    inputLines = T.lines text
    splitAssignments = map (T.splitOn ",") inputLines
    getPairs = map (T.splitOn (T.pack "-")) <$> splitAssignments
    tupleStringPairs = map (unpackTuple . pairFromList) <$> getPairs
    assignments = map (Assignment . rangeFromPair . readTuple) <$> tupleStringPairs
    result = map pairFromList assignments

-- | Returns True if one assignment in the pair completely overlaps the other
hasCompleteOverlap :: AssignmentPair -> Bool
hasCompleteOverlap (Assignment a, Assignment b) = overlap == a || overlap == b
  where
    overlap = a `intersect` b

countTrue :: [Bool] -> Int
countTrue = sum . map fromEnum

solution1 :: T.Text -> Int
solution1 input = (countTrue . map hasCompleteOverlap) $ parseInput input

main0 :: FilePath -> IO ()
main0 path = do
  input <- TIO.readFile path
  print $ solution1 input
