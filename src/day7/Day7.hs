import Data.Function (on)
import Data.List (maximumBy, sort)
import Data.Map (Map, assocs, elems, empty, fromList, insertWith, (!))

newtype Card = Card {name :: Char} deriving (Eq, Show)

cardRank :: Map Char Int
-- PART 1:
-- cardRank = fromList $ zip "23456789TJQKA" [1 ..]

-- PART 2:
cardRank = fromList $ zip "J23456789TQKA" [1 ..]

instance Ord Card where
  (<=) :: Card -> Card -> Bool
  Card x <= Card y = cardRank ! x <= cardRank ! y

counts :: (Ord a) => [a] -> Map a Int
counts = foldr (\x -> insertWith (+) x 1) Data.Map.empty

data HandType = High | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind deriving (Eq, Ord, Show)

calcType :: [Card] -> HandType
calcType cs = case sort $ elems $ counts cs of
  [1, 1, 1, 1, 1] -> High
  [1, 1, 1, 2] -> OnePair
  [1, 2, 2] -> TwoPair
  [1, 1, 3] -> ThreeKind
  [2, 3] -> FullHouse
  [1, 4] -> FourKind
  [5] -> FiveKind

replace' :: (Eq b) => b -> b -> [b] -> [b]
replace' a b = map (\x -> if a == x then b else x)

calcTypeJ :: [Card] -> HandType
calcTypeJ cs =
  let cnt = assocs $ counts $ filter (/= Card 'J') cs

      com = if cnt /= [] then fst $ maximumBy (compare `on` snd) cnt else Card 'K'
      nc = replace' (Card 'J') com cs
   in calcType nc

data Hand = Hand {cards :: [Card], type' :: HandType} deriving (Eq, Show)

instance Ord Hand where
  (<=) :: Hand -> Hand -> Bool
  h1 <= h2 = (type' h1, cards h1) <= (type' h2, cards h2)

parseHand :: String -> Hand
parseHand s =
  let cs = map Card s
      -- PART 1:
      --   t = calcType cs

      -- PART 2:
      t = calcTypeJ cs
   in Hand cs t

parseLine :: String -> (Hand, Int)
parseLine s =
  let [h, b] = words s
   in (parseHand h, read b)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ solve contents

solve :: String -> Int
solve contents =
  let lns = lines contents
      p = map parseLine lns
      rnkd = sort p
   in sum $ zipWith (*) (map snd rnkd) [1 ..]
