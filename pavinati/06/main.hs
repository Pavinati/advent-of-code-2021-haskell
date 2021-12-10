import System.Environment
import Data.List.Split (splitWhen)
import Debug.Trace

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"] -> part1
    ["part2"] -> part2
    _         -> usage

usage :: IO ()
usage = do
  putStrLn "Wrong parameters."
  putStrLn "usage:"
  putStrLn "$  main <part1|part2>"

parseInt :: String -> Int
parseInt = read

groupFishes :: [Int] -> [Int]
groupFishes = foldr incrementAt (take 9 $ repeat 0)

incrementAt :: Int -> [Int] -> [Int]
incrementAt index list =
  let (p1,(x:p2)) = splitAt index list
  in p1 ++ (x + 1) : p2

simulateOneDay :: [Int] -> [Int]
simulateOneDay [g0,g1,g2,g3,g4,g5,g6,g7,g8] =
  [g1, g2, g3, g4, g5, g6, g7 + g0, g8, g0]

simulateXdays :: Int -> [Int] -> [Int]
simulateXdays 0        = id
simulateXdays daysToGo = simulateXdays (daysToGo - 1) . simulateOneDay

solve :: String -> Int
solve input =
  let startingFishes = map parseInt $ splitWhen (==',') input
      fishGroups = groupFishes startingFishes
  in sum $ simulateXdays 80 fishGroups

part1 :: IO ()
part1 = interact $ show . solve

solve' :: String -> Int
solve' input =
  let startingFishes = map parseInt $ splitWhen (==',') input
      fishGroups = groupFishes startingFishes
  in sum $ simulateXdays 256 fishGroups

part2 :: IO ()
part2 = interact $ show . solve'

