import Data.Map (Map, fromList, keys, (!))

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ part1 contents
  print $ part2 contents

part1 :: String -> Int
part1 contents =
  let lns = lines contents
      instrs = head lns
      rest = tail $ tail lns
      ntwrk = fromList $ map parseNetworkLine rest
   in numToZZZ "AAA" (cycle instrs) ntwrk

parseNetworkLine :: String -> (String, (String, String))
parseNetworkLine s =
  let k = take 3 s
      l = take 3 $ drop (length "XXX = (") s
      r = take 3 $ drop (length "XXX = (YYY, ") s
   in (k, (l, r))

numToZZZ :: String -> String -> Map String (String, String) -> Int
numToZZZ "ZZZ" _ _ = 0
numToZZZ curr (instr : instrs) m =
  let nxt = (if instr == 'L' then fst else snd) $ m ! curr
   in 1 + numToZZZ nxt instrs m

numToEndZ :: String -> String -> Map String (String, String) -> Int
numToEndZ s _ _ | last s == 'Z' = 0
numToEndZ curr (instr : instrs) m =
  let nxt = (if instr == 'L' then fst else snd) $ m ! curr
   in 1 + numToEndZ nxt instrs m

part2 :: String -> Int
part2 contents =
  let lns = lines contents
      instrs = head lns
      rest = tail $ tail lns
      ntwrk = fromList $ map parseNetworkLine rest
      currs = filter ((== 'A') . last) (keys ntwrk)
      nums = map (\curr -> numToEndZ curr (cycle instrs) ntwrk) currs
   in foldl lcm 1 nums
