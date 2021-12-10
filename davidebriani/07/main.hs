import Data.List
import Data.Function (on)
import System.Environment (getArgs)

type Position = Double
type Distance = Double
type Step = Double
type Fuel = Double

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"] -> part1
    ["part2"] -> part2
    _         -> usage

part1 :: IO ()
part1 = interact $ show . solve1

part2 :: IO ()
part2 = interact $ show . solve2

usage :: IO ()
usage = do
  putStrLn "Wrong parameters. Usage:"
  putStrLn "$ main <part1|part2>"

solve1 :: String -> Fuel
solve1 input = let positions = sort . parsePositions $ input
                   range = [(head positions)..(last positions)]
                   minimumFuel = minimum $ map (totalFuel distanceConstantCost positions) $ range
                in minimumFuel

solve2 :: String -> Fuel
solve2 input = let positions = sort . parsePositions $ input
                   range = [(head positions)..(last positions)]
                   minimumFuel = minimum $ map (totalFuel distanceIncrementalCost positions) $ range
                in minimumFuel

totalFuel :: (Distance -> Fuel) -> [Position] -> Position -> Fuel
totalFuel fuelCost positions endPosition = sum $ map (fuelCost . (distance endPosition)) $ positions

distanceConstantCost :: Distance -> Fuel
distanceConstantCost = id

-- https://en.wikipedia.org/wiki/1_%2B_2_%2B_3_%2B_4_%2B_%E2%8B%AF
distanceIncrementalCost :: Distance -> Fuel
distanceIncrementalCost distance = distance * (distance + 1) / 2

distance :: Position -> Position -> Distance
distance a b = abs $ b - a

parsePositions :: String -> [Position]
parsePositions = map read . filter (/=",") . groupBy ((==) `on` (==','))
