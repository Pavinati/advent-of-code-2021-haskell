import Data.List
import Data.Function (on)
import System.Environment (getArgs)
import qualified Data.Ord as Ord

type Height = Int
type HeightMap = [[Int]]
type Point = (Int, Int)
type RiskLevel = Int
type Basin = [Point]

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

solve1 :: HeightMap -> RiskLevel
solve1 heightMap = sum . map toRiskLevel . map (toHeight heightMap) . getLowPoints $ heightMap

solve2 :: HeightMap -> Int
solve2 heightMap = product . take 3 . sortOn Ord.Down . map length . map (toBasin heightMap) . getLowPoints $ heightMap

adjacentPoints :: Point -> [Point]
adjacentPoints (i, j) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

getLowPoints :: HeightMap -> [Point]
getLowPoints heightMap = [(i, j) | i <- [0..maxRow], j <- [0..maxCol], isLowPoint heightMap (i, j)]
  where maxRow = (length heightMap) - 1
        maxCol = (length $ head heightMap) - 1

isLowPoint :: HeightMap -> Point -> Bool
isLowPoint heightMap point = all ((> toHeight heightMap point) . toHeight heightMap) $ adjacentPoints point

toRiskLevel :: Height -> RiskLevel
toRiskLevel = (+1)

toHeight :: HeightMap -> Point -> Height
toHeight heightMap (i, j)
  | i >= 0 && i <= maxRow && j >= 0 && j <= maxCol = heightMap !! i !! j
  | otherwise = 9
  where maxRow = (length heightMap) - 1
        maxCol = (length $ head heightMap) - 1

toBasin :: HeightMap -> Point -> Basin
toBasin heightMap lowPoint = findBasin heightMap lowPoint []
  where findBasin heightMap point basin
          | toHeight heightMap point == 9 || point `elem` basin = basin
          | otherwise = findBasin heightMap d basin'''
          where [a, b, c, d] = adjacentPoints point
                basin' = findBasin heightMap a (point : basin)
                basin'' = findBasin heightMap b basin'
                basin''' = findBasin heightMap c basin''

parseLine :: String -> [Height]
parseLine line = [read [char] | char <- line]
