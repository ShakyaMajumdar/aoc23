import Data.Array (Array, Ix (range), bounds, listArray, (!))
import Data.List (tails)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ part1 contents
  print $ part2 contents

part1 :: String -> Int
part1 contents = solve contents 2

part2 :: String -> Int
part2 contents = solve contents 1000000

solve :: String -> Int -> Int
solve contents reps =
  let lns = lines contents
      nr = length lns
      nc = length (head lns)
      grid = listArray ((0, 0), (nr - 1, nc - 1)) (concat lns)
      gxCrds = [ix | ix <- range (bounds grid), grid ! ix == '#']
      emptyRows = [r | r <- [1 .. nr - 1], all (== '.') (getRow grid r)]
      emptyCols = [c | c <- [1 .. nc - 1], all (== '.') (getCol grid c)]
      actualDist g1@(r1, c1) g2@(r2, c2) = 
        rawDist g1 g2 + 
        (reps - 1) * countInRange emptyRows (min r1 r2) (max r1 r2) + 
        (reps - 1) * countInRange emptyCols (min c1 c2) (max c1 c2)
   in sum $ map (uncurry actualDist) (pairs gxCrds)

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

rawDist :: (Int, Int) -> (Int, Int) -> Int
rawDist (r1, c1) (r2, c2) = abs (r2 - r1) + abs (c2 - c1)

countInRange :: [Int] -> Int -> Int -> Int
countInRange xs l r = length $ takeWhile (< r) $ dropWhile (< l) xs

getRow :: Array (Int, Int) a -> Int -> [a]
getRow grid r =
  let (_, (_, cc)) = bounds grid
   in [grid ! (r, c') | c' <- [0 .. cc]]

getCol :: Array (Int, Int) a -> Int -> [a]
getCol grid c =
  let (_, (rr, _)) = bounds grid
   in [grid ! (r', c) | r' <- [0 .. rr]]
