main :: IO ()
main = do 
    contents <- readFile "input.txt"
    print $ part1 contents
    print $ part2 contents

part1 :: String -> Int
part1 s = sum $ map (predict . parseLine) (lines s)

part2 :: String -> Int
part2 s = sum $ map (predict2 . parseLine) (lines s)

parseLine :: String -> [Int]
parseLine s = map read (words s)

predict :: [Int] -> Int
predict xs
    | all (== 0) xs = 0
    | otherwise = (last xs) + predict (diff xs)

diff :: [Int] -> [Int]
diff xs = zipWith (-) (tail xs) xs


predict2 :: [Int] -> Int
predict2 xs
    | all (== 0) xs = 0
    | otherwise = (head xs) - predict2 (diff xs)
