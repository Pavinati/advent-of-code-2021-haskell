module Main where

import Data.List (sort, union, (\\))
import Data.List.Split (splitOn)
import Data.Map (Map, empty, insert, (!))
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1", input] -> part1 input
    ["part2", input] -> part2 input

data Note = Note {input :: [String], output :: [String]} deriving (Show)

lengthIs :: Foldable t1 => (Int -> t2) -> t1 a -> t2
lengthIs p xs = p $ length xs

parse :: String -> Note
parse = toNote . map words . splitOn "|"
  where
    toNote [x, y] = Note {input = x, output = y}

countDigitsWithUniqueNumberOfSegments :: [String] -> Int
countDigitsWithUniqueNumberOfSegments = length . filter (lengthIs (`elem` [2, 3, 4, 7]))

part1 :: FilePath -> IO ()
part1 input = putStrLn . show . sum . map (\n -> countDigitsWithUniqueNumberOfSegments $ output n) . map parse . lines =<< readFile input

data Segment = Up | Down | Mid | UpLeft | UpRight | DownLeft | DownRight deriving (Show, Eq, Ord)

computeMapping :: [String] -> Map Char Segment -> Map Char Segment
computeMapping xs m =
  -- some magic considerations on the structure of 7-segments numbers
  let twoDigits = head . filter (lengthIs (== 2)) $ xs
      threeDigits = head . filter (lengthIs (== 3)) $ xs
      fourDigits = head . filter (lengthIs (== 4)) $ xs
      fiveDigits = filter (lengthIs (== 5)) $ xs
      sixDigits = filter (lengthIs (== 6)) $ xs
      sevenDigits = head . filter (lengthIs (== 7)) $ xs
      cu = threeDigits \\ twoDigits
      cd = head . filter (lengthIs (== 1)) . map (\\ (fourDigits `union` threeDigits)) $ fiveDigits
      cdl = sevenDigits \\ (fourDigits `union` threeDigits `union` cd)
      cul = head . filter (lengthIs (== 1)) . map (\\ (threeDigits `union` cdl `union` cd)) $ sixDigits
      cmid = head . filter (lengthIs (== 1)) . map (\\ (twoDigits `union` cdl `union` cu `union` cd)) $ fiveDigits
      cur = head . filter (\e -> e /= cmid && e /= cdl) . map (sevenDigits \\) $ sixDigits
      cdr = sevenDigits \\ foldl union [] [cu, cd, cdl, cul, cmid, cur]
   in insert (head cdr) DownRight
        . insert (head cur) UpRight
        . insert (head cmid) Mid
        . insert (head cul) UpLeft
        . insert (head cdl) DownLeft
        . insert (head cd) Down
        . insert (head cu) Up
        $ m

toSevenSegment :: Map Char Segment -> String -> [Segment]
toSevenSegment m = sort . map (m !)

toChar :: [Segment] -> Char
-- AoC tells us those are all the cases
toChar [Up, Down, Mid, UpLeft, UpRight, DownRight] = '9'
toChar [Up, Down, Mid, UpLeft, UpRight, DownLeft, DownRight] = '8'
toChar [Up, UpRight, DownRight] = '7'
toChar [Up, Down, Mid, UpLeft, DownLeft, DownRight] = '6'
toChar [Up, Down, Mid, UpLeft, DownRight] = '5'
toChar [Mid, UpLeft, UpRight, DownRight] = '4'
toChar [Up, Down, Mid, UpRight, DownRight] = '3'
toChar [Up, Down, Mid, UpRight, DownLeft] = '2'
toChar [UpRight, DownRight] = '1'
toChar [Up, Down, UpLeft, UpRight, DownLeft, DownRight] = '0'

part2 :: FilePath -> IO ()
part2 i = do
  putStrLn . show . sum . map computeNumber . map parse . lines =<< readFile i
  where
    computeNumber l =
      let m = computeMapping (input l) empty
       in read . map (toChar . toSevenSegment m) $ (output l)
