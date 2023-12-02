import Data.Char (isDigit)
import Data.List ( find, isInfixOf )
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)

type RGB = (Int, Int, Int)

type Game = (Int, [RGB])

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ part1 contents
  print $ part2 contents

part1 :: String -> Int
part1 s = sum $ map fst $ filter isValid $ map parseGame (lines s)

part2 :: String -> Int
part2 s = sum $ map (getPower . parseGame) (lines s)

red :: RGB -> Int
red (r, g, b) = r

green :: RGB -> Int
green (r, g, b) = g

blue :: RGB -> Int
blue (r, g, b) = b

parseGame :: String -> Game
-- s = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
parseGame s = do
  let p = splitOn ": " s
  let id' = read $ drop (length "Game ") (head p) :: Int
  let subs = splitOn "; " (last p)
  (id', map parseSingle subs)

parseSingle :: String -> (Int, Int, Int)
-- s = "3 blue, 4 red"
parseSingle s = do
  let parts = splitOn ", " s
  let red = extractColour "red" parts
  let green = extractColour "green" parts
  let blue = extractColour "blue" parts
  (red, green, blue)

extractColour :: String -> [String] -> Int
extractColour colour parts = extractNum $ find (colour `isInfixOf`) parts

extractNum :: Maybe String -> Int
extractNum Nothing = 0
extractNum (Just s) = read $ takeWhile isDigit s

isValid :: Game -> Bool
isValid (_, subs) = do
  let rmax = maximum $ map red subs
  let gmax = maximum $ map green subs
  let bmax = maximum $ map blue subs
  rmax <= 12 && gmax <= 13 && bmax <= 14

getPower :: Game -> Int
getPower (_, subs) = do
  let rmax = maximum $ map red subs
  let gmax = maximum $ map green subs
  let bmax = maximum $ map blue subs
  rmax * gmax * bmax
