import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ part1 contents

parseLine :: String -> (String, [Int])
parseLine ln =
  let [record, grps] = splitOn " " ln
   in (record, map read $ splitOn "," grps)

part1 :: String -> Int
part1 contents = sum $ map (uncurry (calcWays False False) . parseLine) $ lines contents

calcWays :: Bool -> Bool -> String -> [Int] -> Int
calcWays isOpMust isDamMust rcrd@(r : rs) grps
  | r == '.' && isDamMust = 0
  | r == '.' = calcWays False False rs grps
  | r == '#' && isOpMust = 0
  | r == '#' && null grps = 0
  | r == '#' && head grps > 1 = calcWays False True rs (head grps - 1 : tail grps)
  | r == '#' && head grps == 1 = calcWays True False rs (tail grps)
  | r == '?' && isDamMust = calcWays isOpMust isDamMust ('#' : rs) grps
  | r == '?' && isOpMust = calcWays isOpMust isDamMust ('.' : rs) grps
  | r == '?' = calcWays isOpMust isDamMust ('#' : rs) grps + calcWays isOpMust isDamMust ('.' : rs) grps
calcWays _ _ [] [] = 1
calcWays _ _ [] _ = 0

parseLine2 :: String -> (String, [Int])
parseLine2 contents =
  let (rcrd, grps) = parseLine contents
      rcrd' = intercalate "?" (replicate 5 rcrd)
      grps' = concat (replicate 5 grps)
   in (rcrd', grps')
