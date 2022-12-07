import Data.List (sort)
import qualified System.IO as IO

data Move = Rock | Paper | Scissors deriving (Eq, Show)

data RoundOutcome = Win | Draw | Loss deriving (Eq, Show)

type DesiredOutcome = RoundOutcome

type OpponentMove = Move

type PlayerMove = Move

newtype Round = Round (OpponentMove, PlayerMove) deriving (Show, Eq)

newtype PlannedRound = PlannedRound (OpponentMove, DesiredOutcome)

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

-- X -> Lose
-- Y -> Draw
-- Z -> Win
outcomeFromString :: String -> RoundOutcome
outcomeFromString "X" = Loss
outcomeFromString "Y" = Draw
outcomeFromString "Z" = Win
outcomeFromString [] = error "No Input Provided!"
outcomeFromString p = error "Invalid Input!"

sampleMoves = unlines ["A Y", "B X", "C Z"]

fstPair :: [b] -> (b, b)
fstPair [a, b] = (a, b)
fstPair b = (head b, (head . tail) b)

parseInput :: String -> [(String, String)]
parseInput str = result
  where
    rounds = lines str
    stringifiedMoves = map words rounds
    result = map fstPair stringifiedMoves

pairToRound :: (String, String) -> Round
pairToRound (a, b) = Round (opponent, player)
  where
    opponent = movefromString a
    player = movefromString b

pairsToRounds :: [(String, String)] -> [Round]
pairsToRounds = fmap pairToRound

pairToPlannedRound :: (String, String) -> PlannedRound
pairToPlannedRound (move, outcome) = PlannedRound (opponentMove, desiredOutcome)
  where
    opponentMove = movefromString move
    desiredOutcome = outcomeFromString outcome

pairsToPlannedRounds :: [(String, String)] -> [PlannedRound]
pairsToPlannedRounds = fmap pairToPlannedRound

plannedRoundToRound :: PlannedRound -> Round
plannedRoundToRound (PlannedRound (Rock, Win)) = Round (Rock, Paper)
plannedRoundToRound (PlannedRound (Rock, Loss)) = Round (Rock, Scissors)
plannedRoundToRound (PlannedRound (Paper, Win)) = Round (Paper, Scissors)
plannedRoundToRound (PlannedRound (Paper, Loss)) = Round (Paper, Rock)
plannedRoundToRound (PlannedRound (Scissors, Win)) = Round (Scissors, Rock)
plannedRoundToRound (PlannedRound (Scissors, Loss)) = Round (Scissors, Paper)
plannedRoundToRound (PlannedRound (a, Draw)) = Round (a, a)

plannedRoundstoRounds :: [PlannedRound] -> [Round]
plannedRoundstoRounds = fmap plannedRoundToRound

main1 :: IO ()
main1 = do
  input <- IO.readFile "rounds.txt"
  let scores = map points $ (pairsToRounds . parseInput) input
  print (sum scores)


main2 :: IO ()
main2 = do
  input <- IO.readFile "rounds.txt"
  let scores = map points $ (plannedRoundstoRounds. pairsToPlannedRounds . parseInput) input
  print (sum scores)
