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

type Matrix a = [[a]]

type Board = Matrix (Int, Bool)

mkBoard :: Int -> [Int] -> Board
mkBoard n l = map markUnseen $ chunksOf n l
  where
    markUnseen = map (,False)

markBoard :: Int -> Board -> Board
markBoard n b = [row' | row <- b, let row' = map (markNumber n) row]
  where
    markNumber n (m, _) | n == m = (n, True)
    markNumber _ x = x

hasWon :: Board -> Bool
hasWon b = checkRows b || checkColumns b
  where
    checkRows = check
    checkColumns = check . transpose
    check = any $ all snd

parse :: String -> ([Int], [Board])
parse input = (numbers, boards)
  where
    (numStr : boardsStr) = lines input
    numbers = map (read :: String -> Int) . splitOn "," $ numStr
    boards = (mkBoard 5) <$> map (read :: String -> Int) <$> concatMap words <$> (chunksOf 5 . filter (/= "") $ boardsStr)

play :: [Int] -> [Board] -> (Int, Board)
play (n : ns) boards =
  let boards' = markBoard n <$> boards
   in if any hasWon boards' then (n, (head . filter hasWon) boards') else play ns boards'
play [] _ = (0, mkBoard 0 []) --not happening, we hope

sumUnmarked :: Board -> Int
sumUnmarked = sum . map (sum . map (\(n, b) -> if not b then n else 0))

part1 :: String -> IO ()
part1 filePath =
  putStrLn . show . uncurry computeScore . uncurry play . parse =<< readFile filePath
  where
    computeScore n b = n * sumUnmarked b

play' :: [Int] -> [Board] -> (Int, Board)
play' (n : ns) boards =
  if length boards == 1
    then play (n : ns) boards
    else play' ns nextBoards
  where
    boards' = markBoard n <$> boards
    nextBoards = filter (not . hasWon) boards'

part2 :: FilePath -> IO ()
part2 filePath = do
  putStrLn . show . uncurry computeScore . uncurry play' . parse =<< readFile filePath
  where
    computeScore n b = n * sumUnmarked b