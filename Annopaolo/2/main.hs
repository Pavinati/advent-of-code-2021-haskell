module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"] -> part1
    ["part2"] -> part2
    _ -> usage

usage :: IO ()
usage = do
  putStrLn
    "Wrong parameters. \n\
    \usage:\n\
    \$  main part1\n\
    \  or\n\
    \$  main part2"

data Direction = Forward Int | Down Int | Up Int

stringToDirection :: String -> Direction
stringToDirection s = case words s of
  ["forward", a] -> Forward $ read a
  ["down", a] -> Down $ read a
  ["up", a] -> Up $ read a

finalPosition :: [Direction] -> (Int, Int)
finalPosition = foldl mergeDirection (0, 0)
  where
    mergeDirection (depth, distance) l =
      case l of
        Forward x -> (depth, distance + x)
        Down x -> (depth + x, distance)
        Up x -> (depth - x, distance)

solveWith :: Show a => ([Direction] -> b) -> (b -> a) -> IO ()
solveWith computeFinal mergeFinal = putStrLn . show . mergeFinal . computeFinal . map stringToDirection . lines =<< readFile "input"

mergingWith :: (a -> b) -> a -> b
mergingWith = ($)

part1 :: IO ()
part1 = solveWith finalPosition `mergingWith` multiply
  where
    multiply (x, y) = x * y

finalPosition' :: [Direction] -> (Int, Int, Int)
finalPosition' = foldl mergeDirection (0, 0, 0)
  where
    mergeDirection (depth, distance, aim) l =
      case l of
        Forward x -> (depth + (aim * x), distance + x, aim)
        Down x -> (depth, distance, aim + x)
        Up x -> (depth, distance, aim - x)

part2 :: IO ()
part2 = solveWith finalPosition' `mergingWith` noAim
  where
    noAim (x, y, _) = x * y
