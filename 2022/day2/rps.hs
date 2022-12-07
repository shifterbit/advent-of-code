import Data.List (sort)
import qualified System.IO as IO

data Move = Rock | Paper | Scissors deriving (Eq, Show)

data RoundOutcome = Win | Draw | Loss deriving (Eq, Show)

type OpponentMove = Move

type PlayerMove = Move

newtype Round = Round (OpponentMove, PlayerMove) deriving (Show, Eq)

class Scorable a where
  points :: a -> Int

instance Scorable Move where
  points Rock = 1
  points Paper = 2
  points Scissors = 3

instance Scorable RoundOutcome where
  points Win = 6
  points Draw = 3
  points Loss = 0

instance Scorable Round where
  points round@(Round (opponentMove, playerMove)) = points playerMove + points (roundOutcome round)

roundOutcome :: Round -> RoundOutcome
roundOutcome (Round (Scissors, Scissors)) = Draw
roundOutcome (Round (Scissors, Rock)) = Win
roundOutcome (Round (Scissors, Paper)) = Loss
roundOutcome (Round (Paper, Paper)) = Draw
roundOutcome (Round (Paper, Rock)) = Loss
roundOutcome (Round (Paper, Scissors)) = Win
roundOutcome (Round (Rock, Rock)) = Draw
roundOutcome (Round (Rock, Paper)) = Win
roundOutcome (Round (Rock, Scissors)) = Loss

movefromString :: String -> Move
movefromString "A" = Rock
movefromString "B" = Paper
movefromString "C" = Scissors
movefromString "X" = Rock
movefromString "Y" = Paper
movefromString "Z" = Scissors
movefromString [] = error "No Input Provided!"
movefromString p = error "Invalid Input!"

sampleMoves = unlines ["A Y", "B X", "C Z"]

fstPair :: [b] -> (b, b)
fstPair [a, b] = (a, b)
fstPair b = (head b, (head . tail) b)

parseInput :: String -> [Round]
parseInput str = result
  where
    rounds = lines str
    stringifiedMoves = map words rounds
    result = map (Round . fstPair . map movefromString) stringifiedMoves

main :: IO ()
main = do
  input <- IO.readFile "rounds.txt"
  let scores =  map points (parseInput input)
  print (sum scores)
