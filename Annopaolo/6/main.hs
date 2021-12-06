module Main where

import Data.Char (digitToInt)
import Data.List (group, sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1", input] -> part1 input
    ["part2", input] -> part2 input

parse :: String -> [Int]
parse = map (digitToInt . head) . splitOn ","

type Population = M.Map Int Int

startingMap :: [Int] -> Population
startingMap = M.fromAscList . map (\e -> (head e, length e)) . group . sort

newGeneration :: Population -> Population
newGeneration m =
  M.mapKeys (\k -> if k == (-1) then 8 else k) . addNewBorns . M.mapKeys (subtract 1) $ m
  where
    addNewBorns p = M.insertWith (+) 6 (newBornCount p) p
    newBornCount = M.findWithDefault 0 (-1)

solveFor :: Int -> Population -> Int
solveFor n p = M.foldr (+) 0 $ foldr (\_ pop -> newGeneration pop) p [1 .. n]

part1 :: String -> IO ()
part1 input = putStrLn . show . solveFor 80 . startingMap . parse =<< readFile input

part2 :: String -> IO ()
part2 input = putStrLn . show . solveFor 256 . startingMap . parse =<< readFile input
