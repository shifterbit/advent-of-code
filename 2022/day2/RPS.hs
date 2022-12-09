import Data.Function (on)
import Data.List (sort)
import qualified System.IO as IO

data Move = Rock | Paper | Scissors deriving (Eq, Show)

data RoundOutcome = Win | Draw | Loss deriving (Eq, Show)

type DesiredOutcome = RoundOutcome

type OpponentMove = Move

type PlayerMove = Move

newtype Round = Round (OpponentMove, PlayerMove) deriving (Show, Eq)

newtype RoundPlan = RoundPlan (OpponentMove, DesiredOutcome) deriving (Show, Eq)

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

-- | Return the outcome of the round based on
--  the moves made by the player and opponent
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

-- | Converts a string to an RPS Move
movefromString :: String -> Move
movefromString "A" = Rock
movefromString "B" = Paper
movefromString "C" = Scissors
movefromString "X" = Rock
movefromString "Y" = Paper
movefromString "Z" = Scissors
movefromString [] = error "No Input Provided!"
movefromString p = error "Invalid Input!"

-- | Get the desired outcome based on the string passed
--  X -> Lose
--  Y -> Draw
--  Z -> Win
outcomeFromString :: String -> RoundOutcome
outcomeFromString "X" = Loss
outcomeFromString "Y" = Draw
outcomeFromString "Z" = Win
outcomeFromString [] = error "No Input Provided!"
outcomeFromString p = error "Invalid Input!"

sampleMoves = unlines ["A Y", "B X", "C Z"]

fstPair :: [b] -> (b, b)
fstPair [a, b] = (a, b)
fstPair b = (head b, head $ tail b)

parseInput :: String -> [(String, String)]
parseInput str = result
  where
    rounds = lines str
    stringifiedMoves = map words rounds
    result = map fstPair stringifiedMoves

-- | Converts a String Tuples to a Round
pairToRound :: (String, String) -> Round
pairToRound (a, b) = Round (opponent, player)
  where
    opponent = movefromString a
    player = movefromString b

-- | Converts a List of String Tuples to a list of Rounds
pairsToRounds :: [(String, String)] -> [Round]
pairsToRounds = fmap pairToRound

pairToRoundPlan :: (String, String) -> RoundPlan
pairToRoundPlan (move, outcome) = RoundPlan (opponentMove, desiredOutcome)
  where
    opponentMove = movefromString move
    desiredOutcome = outcomeFromString outcome

pairsToRoundPlans :: [(String, String)] -> [RoundPlan]
pairsToRoundPlans = fmap pairToRoundPlan

-- | Takes a PlannedRound and returns a round that matches the desired result
roundFromRoundPlan :: RoundPlan -> Round
roundFromRoundPlan (RoundPlan (Rock, Win)) = Round (Rock, Paper)
roundFromRoundPlan (RoundPlan (Rock, Loss)) = Round (Rock, Scissors)
roundFromRoundPlan (RoundPlan (Paper, Win)) = Round (Paper, Scissors)
roundFromRoundPlan (RoundPlan (Paper, Loss)) = Round (Paper, Rock)
roundFromRoundPlan (RoundPlan (Scissors, Win)) = Round (Scissors, Rock)
roundFromRoundPlan (RoundPlan (Scissors, Loss)) = Round (Scissors, Paper)
roundFromRoundPlan (RoundPlan (a, Draw)) = Round (a, a)

-- | Takes a List of PlannedRounds and returns a list of Rounds that matches the desired results
roundsFromRoundPlans :: [RoundPlan] -> [Round]
roundsFromRoundPlans = map roundFromRoundPlan

main1 :: IO ()
main1 = do
  input <- IO.readFile "rounds.txt"
  let scores = map points $ (pairsToRounds . parseInput) input
  print (sum scores)

main2 :: IO ()
main2 = do
  input <- IO.readFile "rounds.txt"
  let scores = map points $ (roundsFromRoundPlans . pairsToRoundPlans . parseInput) input
  print (sum scores)
