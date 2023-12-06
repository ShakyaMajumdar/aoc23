import Data.Char (isDigit)
main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ part1 contents
    print $ part2 contents

part1 :: String -> Int
part1 contents = let
    [ts',ds'] = lines contents
    ts = map read $ words $ drop (length "Time:") ts' :: [Int]
    ds = map read $ words $ drop (length "Distance:") ds' :: [Int]
    in
        product $ zipWith numWays ts ds

numWays :: Int -> Int -> Int
numWays t d = length $ filter (>d) [x*(t-x)| x <- [1..t]]

part2 :: String -> Int
part2 contents = let
    [ts',ds'] = lines contents
    t = read $ filter isDigit $ drop (length "Time:") ts' :: Int
    d = read $ filter isDigit $ drop (length "Distance:") ds' :: Int
    a = round ((fromIntegral t - sqrt (fromIntegral (t^2 - 4*d)))/2) + 1
    b = round ((fromIntegral t + sqrt (fromIntegral (t^2 - 4*d)))/2) - 1
    in
        b - a + 1
