{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.List
import Data.List.Split
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1", input] -> part1 input
    ["part2", input] -> part2 input

type Point = (Int, Int)

type Vertices = (Point, Point)

type Line = [Point]

parse :: String -> [Vertices]
parse =
  lines >>> map (splitOn "->" <&> map (splitOn "," >>> (map (read :: String -> Int) >>> tuplify)) <&> tuplify)
  where
    tuplify [a, b] = (a, b)

verticesToLine :: Vertices -> Line
verticesToLine ((x1, y1), (x2, y2))
  | x1 == x2 = (x1,) <$> range y1 y2
  | y1 == y2 = (,y1) <$> range x1 x2
  | otherwise = zip (range x1 x2) (range y1 y2)
  where
    range a b
      | a < b = [a .. b]
      | otherwise = reverse [b .. a]

straight :: Vertices -> Bool
straight ((x1, y1), (x2, y2))
  | x1 == x2 || y1 == y2 = True
  | otherwise = False

countOverlappingPoints :: [Line] -> Int
countOverlappingPoints = length . filter (>= 2) . map length . group . sort . concat

part1 :: FilePath -> IO ()
part1 input = putStrLn . show . countOverlappingPoints . map verticesToLine . filter straight . parse =<< readFile input

part2 :: FilePath -> IO ()
part2 input = putStrLn . show . countOverlappingPoints . map verticesToLine . parse =<< readFile input