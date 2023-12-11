import Data.List.Split
import Data.Map (Map, adjust, fromList, (!))
import Data.Set qualified as S

data Card = Card {id' :: Int, win :: S.Set Int, have :: S.Set Int}

data ProcCard = ProcCard {id'' :: Int, pts :: Int}

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ part1 contents
  print $ part2 contents


-- PART 1 --

part1 :: String -> Int
part1 s = sum $ map (points . parseLine) (lines s)

parseLine :: String -> Card
parseLine s =
  let a = read $ takeWhile (/= ':') $ drop (length "Card ") s :: Int
      rem = tail $ dropWhile (/= ':') s
      rem2 = map words $ splitOn "|" rem
      w = S.fromList $ map read $ head rem2
      h = S.fromList $ map read $ last rem2
   in Card a w h

points :: Card -> Int
points Card {win = w, have = h} =
  let l = length (S.intersection w h)
   in case l of
        0 -> 0
        _ -> 2 ^ (l - 1)


-- PART 2 --

part2 :: String -> Int
part2 s =
  let cards = map (procCard . parseLine) (lines s)
      m = fromList $ zip [1 .. (length cards)] [1, 1 ..]
   in tot cards m

procCard :: Card -> ProcCard
procCard Card {id' = i, win = w, have = h} = ProcCard i (length $ S.intersection w h)

tot :: [ProcCard] -> Map Int Int -> Int
tot (c : cs) m =
  let n = (m ! id'' c)
      newm = foldl (\m i -> adjust (+ n) (id'' c + i) m) m [1 .. pts c]
   in n + tot cs newm
tot [] m = 0
