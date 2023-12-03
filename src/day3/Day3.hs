import Data.Array (Array, Ix (inRange), bounds, indices, listArray, (!))
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ part1 contents
  print $ part2 contents

type Grid = Array (Int, Int) Char

data GNum = GNum {row :: Int, colStart :: Int, len :: Int, value :: Int} deriving (Show)

shiftGNum :: Int -> GNum -> GNum
shiftGNum n gnum = gnum {colStart = colStart gnum + n}

-- PART 1 --
part1 :: String -> Int
part1 s =
  let grid = parseGrid s
      gnums = getGNums grid
   in sum $ map value $ filter (isPart grid) gnums

parseGrid :: String -> Grid
parseGrid s =
  let ls = lines s
      nr = length ls
      nc = length $ head ls
   in listArray ((0, 0), (nr - 1, nc - 1)) $ concat ls

getGNums :: Grid -> [GNum]
getGNums grid =
  let bnds = bounds grid
      nr = 1 + fst (snd bnds)
      nc = 1 + snd (snd bnds)
      getRow r = map (\c -> grid ! (r, c)) [0 .. nc - 1]
   in concatMap (\r -> getGNumsRow (getRow r) r) [0 .. nr - 1]

getGNumsRow :: String -> Int -> [GNum]
getGNumsRow s@(x : xs) r
  | isDigit x =
      let ds = takeWhile isDigit s
          l = length ds
          gn = GNum r 0 l (read ds)
       in gn : map (shiftGNum l) (getGNumsRow (drop l s) r)
  | otherwise = map (shiftGNum 1) (getGNumsRow xs r)
getGNumsRow "" r = []

isSymbol :: Char -> Bool
isSymbol c = c `notElem` "0123456789."

nbrs :: GNum -> [(Int, Int)]
nbrs GNum {row = r, colStart = c, len = l} = [(r - 1, t) | t <- [c - 1 .. c + l]] ++ [(r + 1, t) | t <- [c - 1 .. c + l]] ++ [(r, c - 1), (r, c + l)]

isPart :: Grid -> GNum -> Bool
isPart grid gnum =
  let ns = filter (inRange (bounds grid)) (nbrs gnum)
   in any (isSymbol . (grid !)) ns

-- PART 2 --
part2 :: String -> Int
part2 s =
  let grid = parseGrid s
      gnums = getGNums grid
   in sum $ mapMaybe (getGearRatio gnums) (getGearIndices grid)

getGearIndices :: Grid -> [(Int, Int)]
getGearIndices grid = filter (('*' ==) . (grid !)) (indices grid)

getGearRatio :: [GNum] -> (Int, Int) -> Maybe Int
getGearRatio gnums pos =
  let nbrparts = filter (\gn -> pos `elem` nbrs gn) gnums
   in case nbrparts of
        [x, y] -> Just (value x * value y)
        _ -> Nothing
