import Data.List
import System.Environment (getArgs)

type Energy = Int
type EnergyGrid = [[Energy]]
type Point = (Int, Int)
type FlashedPoint = Point
type FlashCount = Int

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"] -> part1
    ["part2"] -> part2
    _         -> usage

part1 :: IO ()
part1 = interact $ show . solve1 . map parseLine . lines

part2 :: IO ()
part2 = interact $ show . solve2 . map parseLine . lines

usage :: IO ()
usage = do
  putStrLn "Wrong parameters. Usage:"
  putStrLn "$ main <part1|part2>"

solve1 :: EnergyGrid -> FlashCount
solve1 = snd . countFlashesAfterSteps 100

solve2 :: EnergyGrid -> Int
solve2 = length . takeWhile (not . allFlashing) . doIterateSteps

countFlashesAfterSteps :: Int -> EnergyGrid -> (EnergyGrid, FlashCount)
countFlashesAfterSteps n grid = (iterate countFlashesAfterStep (grid, 0)) !! n

countFlashesAfterStep :: (EnergyGrid, FlashCount) -> (EnergyGrid, FlashCount)
countFlashesAfterStep (grid, flashCount) = (updatedGrid, updatedFlashCount)
  where (updatedGrid, flashedPoints) = doStep (grid, [])
        updatedFlashCount = flashCount + length flashedPoints

doIterateSteps :: EnergyGrid -> [EnergyGrid]
doIterateSteps = iterate (doDiscardFlashedPoints . doStep . doAddFlashedPoints)

doStep :: (EnergyGrid, [FlashedPoint]) -> (EnergyGrid, [FlashedPoint])
doStep = doResetEnergy . doFlashes . doIncreaseEnergy

doIncreaseEnergy :: (EnergyGrid, [FlashedPoint]) -> (EnergyGrid, [FlashedPoint])
doIncreaseEnergy (grid, flashedPoints) = (map (map (+1)) grid, flashedPoints)

doFlashes :: (EnergyGrid, [FlashedPoint]) -> (EnergyGrid, [FlashedPoint])
doFlashes (grid, flashedPoints)
  | null flashingPoints = (grid, flashedPoints)
  | otherwise = doFlashes (updatedGrid, updatedFlashedPoints)
  where flashingPoints = [p | p <- gridPoints grid, grid `at` p > 9, p `notElem` flashedPoints]
        updatedGrid = foldl increaseEnergyForAdjacents grid flashingPoints
        updatedFlashedPoints = flashedPoints ++ flashingPoints

doResetEnergy :: (EnergyGrid, [FlashedPoint]) -> (EnergyGrid, [FlashedPoint])
doResetEnergy (grid, flashedPoints) = (updatedGrid, flashedPoints)
  where updatedGrid = foldl resetEnergyForPoint grid flashedPoints

doDiscardFlashedPoints :: (EnergyGrid, [FlashedPoint]) -> EnergyGrid
doDiscardFlashedPoints (grid, flashedPoints) = grid

doAddFlashedPoints :: EnergyGrid -> (EnergyGrid, [FlashedPoint])
doAddFlashedPoints grid = (grid, [])

resetEnergyForPoint :: EnergyGrid -> Point -> EnergyGrid
resetEnergyForPoint = updatePoint (\x -> 0)

increaseEnergyForAdjacents :: EnergyGrid -> Point -> EnergyGrid
increaseEnergyForAdjacents grid point = foldl (updatePoint (+ 1)) grid $ adjacentPoints grid point

adjacentPoints :: EnergyGrid -> Point -> [Point]
adjacentPoints grid (i, j) = adjacent
    where ninePoints = [(i + di, j + dj) | di <- [-1 .. 1], dj <- [-1 .. 1]]
          adjacent = filter (`isWithin` grid) . filter (/= (i, j)) $ ninePoints

updatePoint :: (Energy -> Energy) -> EnergyGrid -> Point -> EnergyGrid
updatePoint f grid point = [[if point == (i, j) then f (grid `at` (i, j)) else grid `at` (i, j) | j <- [0..maxCol]] | i <- [0..maxRow]]
  where maxRow = (length grid) - 1
        maxCol = (length $ head grid) - 1

gridPoints :: EnergyGrid -> [Point]
gridPoints grid = [(i, j) | i <- [0..maxRow], j <- [0..maxCol]]
  where maxRow = (length grid) - 1
        maxCol = (length $ head grid) - 1

isWithin :: Point -> EnergyGrid -> Bool
point `isWithin` grid = point `elem` (gridPoints grid)

at :: EnergyGrid -> Point -> Energy
grid `at` (i, j) = grid !! i !! j

allFlashing :: EnergyGrid -> Bool
allFlashing = all (== 0) . concat

parseLine :: String -> [Energy]
parseLine line = [read [x] | x <- line]
