import Control.Monad (foldM)
import Data.List (foldl1', intersect, nub)
import qualified Data.Map as M
import System.IO (readFile')

newtype Compartment = Compartment String deriving (Show)

newtype Rucksack = Rucksack (Compartment, Compartment) deriving (Show)

newtype Group = Group [Maybe Rucksack] deriving (Show)

sampleInput =
  unlines
    [ "vJrwpWtwJgWrhcsFMMfFFhFp",
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
      "PmmdzqPrVvPwwTWBwg",
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
      "ttgJtRGJQctTZtZT",
      "CrZsJsPPZsGzwwsLwLmpwMDw"
    ]

priorityTable = M.fromList $ zip [' '] [0] ++ zip ['a' .. 'z'] [1 .. 26] ++ zip ['A' .. 'Z'] [27 .. 52]

mkRucksack :: (String, String) -> Rucksack
mkRucksack (a, b) = Rucksack (Compartment a, Compartment b)

parseInput :: String -> [Rucksack]
parseInput input = map mkRucksack compartmentPairs
  where
    splitInput = lines input
    splitHalf xs = splitAt (length xs `div` 2) xs
    compartmentPairs = map splitHalf splitInput

main1 :: FilePath -> IO ()
main1 path = do
  input <- readFile' path
  let compartments = duplicateCompartments $ parseInput input
  let result = sumPriorities compartments
  print result

main2 :: FilePath -> IO ()
main2 path = do
  input <- readFile' path
  let groups = groupRucksacks $ parseInput input
  let result = sumPriorities $ groupBadges groups
  print result

-- | Part 1 Solution
solutionPart1 = main1 "rucksacks.txt"

-- | Part 2 Solution
solutionPart2 = main2 "rucksacks.txt"

duplicateCompartment :: Rucksack -> Char
duplicateCompartment (Rucksack (Compartment a, Compartment b)) = head (nub (a `intersect` b))

duplicateCompartments :: [Rucksack] -> [Char]
duplicateCompartments = map duplicateCompartment

compartmentPriority :: Char -> Maybe Integer
compartmentPriority a = M.lookup a priorityTable

compartmentPriorities :: [Char] -> [Maybe Integer]
compartmentPriorities = map compartmentPriority

sumPriorities :: String -> Maybe Integer
sumPriorities a = sum <$> (sequence . compartmentPriorities) a

groupRucksacks :: [Rucksack] -> [Group]
groupRucksacks [] = []
groupRucksacks [a] = [Group [Just a, Nothing, Nothing]]
groupRucksacks [a, b] = [Group [Just a, Just b, Nothing]]
groupRucksacks (a : b : c : xs) = Group [Just a, Just b, Just c] : groupRucksacks xs

rucksackToString :: Rucksack -> String
rucksackToString (Rucksack (Compartment a, Compartment b)) = a ++ b

rucksacksToStrings :: [Rucksack] -> [String]
rucksacksToStrings = map rucksackToString

groupToString :: Group -> Maybe [String]
groupToString (Group rucksacks) = rucksacksToStrings <$> sequence rucksacks

groupBadge :: Group -> Char
groupBadge group = uniqueLetters groupStrings
  where
    groupStrings = groupToString group
    uniqueLetters (Just a) = (head . nub) $ foldl1' intersect a
    uniqueLetters Nothing = ' '

groupBadges :: [Group] -> [Char]
groupBadges = map groupBadge
