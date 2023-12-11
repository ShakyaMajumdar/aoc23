import Data.Char (isDigit)
import Data.List (find, isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)

type RGB = (Int, Int, Int)

red :: RGB -> Int
red (r, g, b) = r

green :: RGB -> Int
green (r, g, b) = g

blue :: RGB -> Int
blue (r, g, b) = b

type Game = (Int, [RGB])

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ part1 contents
  print $ part2 contents

-- PART 1 --
part1 :: String -> Int
part1 s = sum $ map fst $ filter isValid $ map parseGame (lines s)

parseGame :: String -> Game
-- s = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
parseGame s =
  let p = splitOn ": " s
      id' = read $ drop (length "Game ") (head p) :: Int
      subs = splitOn "; " (last p)
   in (id', map parseSingle subs)

parseSingle :: String -> (Int, Int, Int)
-- s = "3 blue, 4 red"
parseSingle s =
  let parts = splitOn ", " s
      red = extractColour "red" parts
      green = extractColour "green" parts
      blue = extractColour "blue" parts
   in (red, green, blue)

extractColour :: String -> [String] -> Int
extractColour colour parts = extractNum $ find (colour `isInfixOf`) parts

extractNum :: Maybe String -> Int
extractNum Nothing = 0
extractNum (Just s) = read $ takeWhile isDigit s

isValid :: Game -> Bool
isValid (_, subs) =
  let most c = maximum $ map c subs
   in (most red <= 12) && (most green <= 13) && (most blue <= 14)

-- PART 2 --
part2 :: String -> Int
part2 s = sum $ map (getPower . parseGame) (lines s)

getPower :: Game -> Int
getPower (_, subs) =
  let most c = maximum $ map c subs
   in most red * most blue * most green
