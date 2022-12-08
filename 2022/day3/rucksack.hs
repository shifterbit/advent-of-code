import Data.List (intersect, nub)
import qualified Data.Map as M
import System.IO (readFile')

newtype Compartment = Compartment String deriving (Show)

newtype Rucksack = Rucksack (Compartment, Compartment) deriving (Show)

sampleInput =
  unlines
    [ "vJrwpWtwJgWrhcsFMMfFFhFp",
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
      "PmmdzqPrVvPwwTWBwg",
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
      "ttgJtRGJQctTZtZT",
      "CrZsJsPPZsGzwwsLwLmpwMDw"
    ]

priorityTable = M.fromList $ zip ['a' .. 'z'] [1 .. 26] ++ zip ['A' .. 'Z'] [27 .. 52]

mkRucksack :: (String, String) -> Rucksack
mkRucksack (a, b) = Rucksack (Compartment a, Compartment b)

parseInput :: String -> [Rucksack]
parseInput input = map mkRucksack compartmentPairs
  where
    splitInput = lines input
    splitHalf xs = splitAt (length xs `div` 2) xs
    compartmentPairs = map splitHalf splitInput

main0 :: FilePath -> IO (Maybe Integer)
main0 path = do
    input <- readFile' path
    return $ sumPriorities input



duplicateCompartment :: Rucksack -> Char
duplicateCompartment (Rucksack (Compartment a, Compartment b)) = head (nub (a `intersect` b))

duplicateCompartments :: [Rucksack] -> [Char]
duplicateCompartments = map duplicateCompartment

compartmentPriority :: Char -> Maybe Integer
compartmentPriority a = M.lookup a priorityTable

compartmentPriorities :: [Char] -> [Maybe Integer]
compartmentPriorities = map compartmentPriority

sumPriorities :: String -> Maybe Integer
sumPriorities a = sum <$> (sequence . compartmentPriorities . duplicateCompartments . parseInput) a
