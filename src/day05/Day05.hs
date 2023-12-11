import Data.Function (on)
import Data.List (find, minimumBy, sortOn)
import Data.Maybe (listToMaybe)

data DSN = DSN {d :: Int, s :: Int, n :: Int} deriving (Show, Eq)

data LR = LR {l :: Int, r :: Int} | ZeroLr deriving (Show, Eq)

listToLr :: [LR] -> LR
listToLr [x] = x
listToLr [] = ZeroLr

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ part1 contents
  print $ part2 contents

part1 :: String -> Int
part1 s =
  let lns = lines s
      se = parseSeeds lns
      se2so = parseWithHeader "seed-to-soil map:" lns
      so2fe = parseWithHeader "soil-to-fertilizer map:" lns
      fe2wa = parseWithHeader "fertilizer-to-water map:" lns
      wa2li = parseWithHeader "water-to-light map:" lns
      li2te = parseWithHeader "light-to-temperature map:" lns
      te2hu = parseWithHeader "temperature-to-humidity map:" lns
      hu2lo = parseWithHeader "humidity-to-location map:" lns
      compLookup = foldl (.) id (map lookup' [hu2lo, te2hu, li2te, wa2li, fe2wa, so2fe, se2so])
   in minimum $ map compLookup se

parseSeeds :: [String] -> [Int]
parseSeeds lns = map read $ words $ drop (length "seeds: ") (head lns)

parseTriple :: String -> DSN
parseTriple l =
  let a = map read $ words l
   in DSN (head a) (a !! 1) (a !! 2)

parseWithHeader :: String -> [String] -> [DSN]
parseWithHeader h lns =
  let lns' = takeWhile (/= "") $ tail $ dropWhile (/= h) lns
      t = map parseTriple lns'
   in sortOn s t

lookup' :: [DSN] -> Int -> Int
lookup' mp src =
  let fnd = find (\dsn -> s dsn <= src && src < s dsn + n dsn) mp
   in maybe src (\dsn -> d dsn + (src - s dsn)) fnd

part2 :: String -> Int
part2 contents =
  let lns = lines contents
      se = sortOn l $ parseSeedRange $ parseSeeds lns
      se2so = parseWithHeader "seed-to-soil map:" lns
      so2fe = parseWithHeader "soil-to-fertilizer map:" lns
      fe2wa = parseWithHeader "fertilizer-to-water map:" lns
      wa2li = parseWithHeader "water-to-light map:" lns
      li2te = parseWithHeader "light-to-temperature map:" lns
      te2hu = parseWithHeader "temperature-to-humidity map:" lns
      hu2lo = parseWithHeader "humidity-to-location map:" lns
      compLookup = foldl (.) id (map applyMapToMany [hu2lo, te2hu, li2te, wa2li, fe2wa, so2fe, se2so])
   in l $ head $ compLookup se

parseSeedRange :: [Int] -> [LR]
parseSeedRange (x : y : xs) = LR x (x + y - 1) : parseSeedRange xs
parseSeedRange [] = []

applyDsn :: DSN -> Int -> Int
applyDsn DSN {d = d', s = s', n = n'} x = d' + (x - s')

applyDsnLr :: DSN -> LR -> LR
applyDsnLr dsn LR {l = l', r = r'} = LR (applyDsn dsn l') (applyDsn dsn r')

shiftL :: DSN -> Int -> DSN
shiftL DSN {d = d', s = s', n = n'} l' = DSN (d' + (l' - s')) l' (n' - (l' - s'))

shiftR :: DSN -> Int -> DSN
shiftR DSN {d = d', s = s', n = n'} r' = DSN d' s' (r'-s'+1)

slr :: DSN -> LR
slr DSN {s = s', n = n'} = LR s' (s' + n' - 1)

f :: DSN -> LR -> ([LR], LR)
f _ ZeroLr = ([], ZeroLr)
f dsn@DSN {d = d', s = s', n = n'} lr@LR {l = l', r = r'}
  | s' > r' = ([lr], ZeroLr)
  | s' + n' - 1 < l' = ([], lr)
  | s' < l' = f (shiftL dsn l') lr
  | s' + n' - 1 > r' = f (shiftR dsn r') lr
  | otherwise = ([LR l' (s'-1) | l' /= s'] ++ [applyDsnLr dsn $ LR s' (s'+n'-1)], listToLr [LR (s'+n') r' | s'+n'-1 /= r']) -- [LR (s'+n') r' | kr && (s'+n'-1)/=r'], LR )

applyMap :: [DSN] -> LR -> ([LR], LR)
applyMap dsns lr = foldl (\(p,u) d -> let (p', u') = f d u in (p++p',u')) ([], lr) dsns

applyMapToMany :: [DSN] -> [LR] -> [LR]
applyMapToMany dsns lrs = sortOn l $ concatMap (collect . applyMap dsns) lrs

collect :: ([LR], LR) -> [LR]
collect (lrs, ZeroLr) = lrs
collect (lrs, lr) = lrs ++ [lr]
