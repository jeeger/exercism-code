module ZebraPuzzle (Resident(..), Solution(..), solve) where
import Data.List(permutations, find, zipWith6)
import Control.Monad(guard)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)


type HouseCheck = House -> Bool
type StateCheck = State -> Bool

newtype State = State [House] deriving (Eq, Show)
data House = House Int (Maybe Resident) (Maybe Color) (Maybe Pet) (Maybe Drink) (Maybe Cigarette) deriving (Eq, Show)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese deriving (Eq, Show, Enum)
data Color = Red | Green | Ivory | Yellow | Blue deriving (Show, Enum, Eq)
data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments deriving (Show, Enum, Eq)
data Pet = Dog | Snails | Fox | Horse | Zebra deriving (Show, Enum, Eq)
data Drink = Coffee | Tea | Milk | OrangeJuice | Water deriving (Show, Enum, Eq)

-- Choices per first house: 5^4.
-- Choices second house: 4^4
-- choices third house: 3^4
-- choices fourth house: 2^4
-- choices fifth house: 1


residentChoices :: [[Resident]]
residentChoices = permutations [Englishman .. Japanese]
colorChoices :: [[Color]]
colorChoices = permutations [Red .. Blue]
petChoices :: [[Pet]]
petChoices = permutations [Dog .. Zebra]
drinkChoices :: [[Drink]]
drinkChoices = permutations [Coffee .. Water]
cigaretteChoices :: [[Cigarette]]
cigaretteChoices = permutations [OldGold .. Parliaments]

-- This is way too slow.
allChoices:: [State]
allChoices = do
  res <- residentChoices
  col <- colorChoices
  pet <- petChoices
  drink <- drinkChoices
  cig <- cigaretteChoices
  let state = State $ createState res col pet drink cig
  guard (checkSolution state)
  return state where
    createState:: [Resident] -> [Color] -> [Pet] -> [Drink] -> [Cigarette] -> [House]
    createState = zipWith6 House [1..5]


-- Each of the five houses is painted a different color, and their inhabitants are of different national extractions, own different pets, drink different beverages and smoke different brands of cigarettes.

-- There are five houses.
-- The Englishman lives in the red house.

englishRed :: House -> Bool
englishRed (House _ Englishman Red _ _ _) = True
englishRed _ = False

-- The Spaniard owns the dog.

spaniardDog :: House -> Bool
spaniardDog (House _ Spaniard _ Dog _ _) = True
spaniardDog _ = False

-- Coffee is drunk in the green house.
greenTea :: House -> Bool
greenTea (House _ _ Green _ Tea _) = True
greenTea _ = False

-- The Ukrainian drinks tea.

ukrainianTea :: House -> Bool
ukrainianTea (House _ Ukrainian _ _ Tea _) = True
ukrainianTea _ = False

-- The Old Gold smoker owns snails.
goldSnails :: House -> Bool
goldSnails (House _ _ _ Snails _ OldGold) = True
goldSnails _ = False

-- Kools are smoked in the yellow house.

koolsYellow :: House -> Bool
koolsYellow (House _ _ Yellow _ _ Kools) = True
koolsYellow _ = False

-- Milk is drunk in the middle house.

milkMiddle :: House -> Bool
milkMiddle (House 3 _ _ _ Milk _)  = True
milkMiddle _ = False

-- The Norwegian lives in the first house.

norwegianFirst :: House -> Bool
norwegianFirst (House 1 Norwegian _ _ _ _) = True
norwegianFirst _ = False

-- The Lucky Strike smoker drinks orange juice.
luckyOrange :: House -> Bool
luckyOrange (House _ _ _ _ OrangeJuice LuckyStrike) = True
luckyOrange _ = False
-- The Japanese smokes Parliaments.
japLiament :: House -> Bool
japLiament (House _ Japanese _ _ _ Parliaments) = True
japLiament _ = False

-- The Norwegian lives next to the blue house.
norBlue:: StateCheck
norBlue (State h) = case (find norwegian h) of
  Nothing -> False
  Just (House index _ _ _ _ _ ) -> any (\(House i _ color _ _ _ ) -> (i == index - 1 || i == index + 1) && color == Blue) h
  where
    norwegian (House _ Norwegian _ _ _ _) = True
    norwegian _ = False

-- The green house is immediately to the right of the ivory house.
greenIvory:: StateCheck
greenIvory (State h) = case (find ivory h) of
  Nothing -> False
  Just (House index _ _ _ _ _) -> any (\(House i _ color _ _ _) -> (i == index + 1) && color == Green) h
  where
    ivory (House _ _ Ivory _ _ _ ) = True
    ivory _ = False

-- Kools are smoked in the house next to the house where the horse is kept.
horseKools:: StateCheck
horseKools (State h) = case (find horse h) of
  Nothing -> False
  Just (House index _ _ _ _ _) -> any (\(House i _ _ _ _ cigs) -> (i == index + 1 || i == index - 1) && cigs == Kools) h
  where
    horse (House _ _ _ Horse _ _ ) = True
    horse _ = False

-- The man who smokes Chesterfields lives in the house next to the man with the fox.
chesterFox:: StateCheck
chesterFox (State h) = case find fox h of
  Nothing -> False
  Just (House index _ _ _ _ _) -> any (\(House i _ _ _ _ cigs) -> (i == index + 1 || i == index -1) && cigs == Chesterfields) h
  where
    fox (House _ _ _ Fox _ _) = True
    fox _ = False

makeStateCheck:: HouseCheck -> StateCheck
makeStateCheck h (State s) = any h s

allChecks :: [StateCheck]
allChecks = fmap makeStateCheck [englishRed, spaniardDog, greenTea, ukrainianTea, goldSnails, koolsYellow, milkMiddle, norwegianFirst, luckyOrange, japLiament]
            ++ [norBlue, greenIvory, horseKools, chesterFox]

checkSolution:: State -> Bool
checkSolution s = go allChecks where
  go [] = True
  go (c:cs) = c s && go cs

solve :: Solution
solve = undefined
