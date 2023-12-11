import Data.List
import Data.Function
import Data.Char

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ part1 contents
    print $ part2 contents

-- PART 1 --
extractNumeric :: String -> [Int]
extractNumeric s = map digitToInt $ filter (`elem` ['1'..'9']) s

getCalib1 :: [Int] -> Int
getCalib1 s = head s * 10 + last s

part1 :: String -> Int
part1 contents = sum $ map (getCalib1 . extractNumeric) (words contents)

-- PART 2 --
index :: String -> String -> Maybe Int
index haystack needle | needle `isPrefixOf` haystack = Just 0
index (_:haystack) needle = fmap (1+) (index haystack needle)
index [] needle = Nothing

rindex :: String -> String -> Maybe Int
rindex haystack needle = index (reverse haystack) (reverse needle)

sDigitToInt :: String -> Int
sDigitToInt "one" = 1
sDigitToInt "two" = 2
sDigitToInt "three" = 3
sDigitToInt "four" = 4
sDigitToInt "five" = 5
sDigitToInt "six" = 6
sDigitToInt "seven" = 7
sDigitToInt "eight" = 8
sDigitToInt "nine" = 9
sDigitToInt s | s `isInfixOf` "123456789" = read s

sDigits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

getFirst2 :: String -> Int
getFirst2 s = sDigitToInt $ minimumBy (compare `on` index s) (filter (`isInfixOf` s) sDigits)

getLast2 :: String -> Int
getLast2 s = sDigitToInt $ minimumBy (compare `on` rindex s) (filter (`isInfixOf` s) sDigits)

getCalib2 :: String -> Int
getCalib2 s = getFirst2 s * 10 + getLast2 s

part2 :: String -> Int
part2 contents = sum $ map getCalib2 (words contents)
