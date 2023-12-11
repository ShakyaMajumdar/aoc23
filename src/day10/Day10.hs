import Data.Array (Array, Ix (inRange, range), assocs, bounds, elems, listArray, (!), (//))
import Data.Maybe (catMaybes, mapMaybe)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ part1 contents
  print $ part2 contents

part1 :: String -> Int
part1 contents =
  let grid = parseGrid contents
      bnds = bounds grid
      [start] = map fst $ filter ((== Start) . snd) (assocs grid)
      dists = listArray bnds [0 | _ <- range bnds]
      vis = listArray bnds [False | _ <- range bnds]
   in maximum $ makeDists grid [start] dists vis

data Tile = NS | EW | NE | NW | SW | SE | Gnd | Start deriving (Eq, Show)

type Grid = Array (Int, Int) Tile

parseTile :: Char -> Tile
parseTile '|' = NS
parseTile '-' = EW
parseTile 'L' = NE
parseTile 'J' = NW
parseTile '7' = SW
parseTile 'F' = SE
parseTile '.' = Gnd
parseTile 'S' = Start

parseGrid :: String -> Grid
parseGrid s =
  let lns = lines s
      nr = length lns
      nc = length $ head lns
   in listArray ((0, 0), (nr - 1, nc - 1)) $ concatMap (map parseTile) lns

makeDists :: Grid -> [(Int, Int)] -> Array (Int, Int) Int -> Array (Int, Int) Bool -> Array (Int, Int) Int
makeDists grid [] dists vis = dists
makeDists grid ((r, c) : q) dists vis =
  let [up, down, left, right] = [((r - 1, c), [NS, SW, SE]), ((r + 1, c), [NS, NW, NE]), ((r, c - 1), [EW, NE, SE]), ((r, c + 1), [EW, NW, SW])]
      nbrs = case grid ! (r, c) of
        Start -> [up, down, left, right]
        NS -> [up, down]
        EW -> [left, right]
        NE -> [up, right]
        NW -> [up, left]
        SW -> [down, left]
        SE -> [down, right]
      ns = mapMaybe (\(n, v) -> if inRange (bounds grid) n && not (vis ! n) && (grid ! n) `elem` v then Just n else Nothing) nbrs
      newdists = dists // [(n, (dists ! (r, c)) + 1) | n <- ns]
   in makeDists grid (q ++ ns) newdists (vis // [(n, True) | n <- ns])

part2 :: String -> Int
part2 contents =
  let grid = parseGrid contents
      bnds = bounds grid
      [start] = map fst $ filter ((== Start) . snd) (assocs grid)
      dists = makeDists grid [start] (listArray bnds [0 | _ <- range bnds]) (listArray bnds [False | _ <- range bnds])
      grid' = grid // ((start, EW) : [(p, Gnd) | p <- range bnds, dists ! p == 0])
   in length $ filter (\ix -> grid' ! ix == Gnd && f grid' ix) $ range (bounds grid')

countNS :: [Tile] -> (Int, Int)
countNS (x : xs) =
  let (restN, restS) = countNS xs
   in case x of
        NS -> (restN + 1, restS + 1)
        EW -> (restN, restS)
        NE -> (restN + 1, restS)
        NW -> (restN + 1, restS)
        SW -> (restN, restS + 1)
        SE -> (restN, restS + 1)
        Gnd -> (restN, restS)
countNS [] = (0, 0)

countEW :: [Tile] -> (Int, Int)
countEW (x : xs) =
  let (restE, restW) = countEW xs
   in case x of
        EW -> (restE + 1, restW + 1)
        NS -> (restE, restW)
        NE -> (restE + 1, restW)
        SE -> (restE + 1, restW)
        NW -> (restE, restW + 1)
        SW -> (restE, restW + 1)
        Gnd -> (restE, restW)
countEW [] = (0, 0)

f :: Grid -> (Int, Int) -> Bool
f grid ix =
  let (_, (nr', nc')) = bounds grid
      (nr, nc) = (nr' + 1, nc' + 1)
      (r, c) = ix
      left = [grid ! (r, c') | c' <- [0 .. c - 1]]
      up = [grid ! (r', c) | r' <- [0 .. r - 1]]
      down = [grid ! (r', c) | r' <- [r + 1 .. nr - 1]]
      right = [grid ! (r, c') | c' <- [c + 1 .. nc - 1]]
      lprs = uncurry min $ countNS left
      uprs = uncurry min $ countEW up
      rprs = uncurry min $ countNS right
      dprs = uncurry min $ countEW down
      isOdd x = x `mod` 2 == 1
   in all isOdd [lprs, uprs, rprs, dprs]
