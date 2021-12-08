import Data.List
import Data.Function (on)
import System.Environment (getArgs)

type Fish = Int
type FishCount = Int

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"] -> part1
    ["part2"] -> part2
    _         -> usage

part1 :: IO ()
part1 = interact $ show . sum . afterDays 80 . countFishesByDays . parseFishes

part2 :: IO ()
part2 = interact $ show . sum . afterDays 256 . countFishesByDays . parseFishes

usage :: IO ()
usage = do
  putStrLn "Wrong parameters. Usage:"
  putStrLn "$ main <part1|part2>"

afterDays :: Int -> [FishCount] -> [FishCount]
afterDays 0 fishCounts = fishCounts
afterDays n [d0, d1, d2, d3, d4, d5, d6, d7, d8] = afterDays (n -1) [d1, d2, d3, d4, d5, d6, d0 + d7, d8, d0]

countFishesByDays :: [Fish] -> [FishCount]
countFishesByDays = map pred . map length . group . sort . (++[0,1,2,3,4,5,6,7,8])

parseFishes :: String -> [Fish]
parseFishes = map read . filter (/=",") . groupBy ((==) `on` (==','))
