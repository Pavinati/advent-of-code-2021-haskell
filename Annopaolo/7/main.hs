{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import Data.List.Split
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1", input] -> part1 input
    ["part2", input] -> part2 input

fuelWith :: (Num c, Num b) => (b -> c) -> b -> [b] -> c
fuelWith price to = sum . map (price . abs . subtract to)

solve :: (Ord a, Ord b, Enum b, Num a, Num b) => (b -> a) -> [b] -> a
solve price ps = minimum [fuelWith price dest ps | dest <- [minimum ps .. maximum ps]]

parse :: String -> [Int]
parse = map read . splitOn ","

part1 :: FilePath -> IO ()
part1 filePath = putStrLn . show . solve id . parse =<< readFile filePath

trueCrabPrice :: Real a => a -> Rational
trueCrabPrice m = toRational (m * (m + 1)) / 2

part2 :: FilePath -> IO ()
part2 filePath = putStrLn . show . solve trueCrabPrice . parse =<< readFile filePath
