import Data.List
import Data.Char (isLower)
import Data.Maybe (fromJust)
import qualified Data.Map as Map  
import System.Environment (getArgs)

type Cave = String
type CaveMap = Map.Map Cave [Cave]
type Path = [Cave]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"] -> part1
    ["part2"] -> part2
    _         -> usage

part1 :: IO ()
part1 = interact $ show . solve1 . parseCaveMap . lines

part2 :: IO ()
part2 = interact $ show . solve2 . parseCaveMap . lines

usage :: IO ()
usage = do
  putStrLn "Wrong parameters. Usage:"
  putStrLn "$ main <part1|part2>"

solve1 :: CaveMap -> Int
solve1 = length . backtrackPaths [["end"]] isValidWith1Visit

solve2 :: CaveMap -> Int
solve2 = length . backtrackPaths [["end"]] isValidWith2Visits

backtrackPaths :: [Path] -> (Path -> Bool) -> CaveMap -> [Path]
backtrackPaths paths isValid caveMap
  | completePaths == paths = completePaths
  | otherwise = completePaths ++ backtrackPaths incompletePathExtensions isValid caveMap
  where completePaths = filter isComplete paths
        incompletePaths = paths \\ completePaths
        incompletePathExtensions = concat . map (filter isValid . extendPath caveMap) $ incompletePaths

extendPath :: CaveMap -> Path -> [Path]
extendPath caveMap path = pathExtensions
  where latestCave = head path
        nextCaves = fromJust $ Map.lookup latestCave caveMap
        pathExtensions = (\nextCave -> nextCave:path) <$> nextCaves

isComplete :: Path -> Bool
isComplete ("start":path) = True
isComplete _ = False

isValidWith1Visit :: Path -> Bool
isValidWith1Visit path = visitsCavesOnce
  where smallCaves = filter isSmall path
        smallCavesCount = length smallCaves
        uniqueSmallCavesCount = length $ nub smallCaves
        visitsCavesOnce = smallCavesCount == uniqueSmallCavesCount

isValidWith2Visits :: Path -> Bool
isValidWith2Visits path = visitsCavesOnce || visitsOneMiddleCaveTwice
  where smallCaves = filter isSmall path
        smallCavesCount = length smallCaves
        uniqueSmallCavesCount = length $ nub smallCaves
        visitsCavesOnce = smallCavesCount == uniqueSmallCavesCount
        visitsOneCaveTwice = smallCavesCount == uniqueSmallCavesCount + 1
        visitsStartOnce = (<2) . length . filter (=="start") $ path
        visitsEndOnce = (<2) . length . filter (=="end") $ path
        visitsOneMiddleCaveTwice = visitsOneCaveTwice && visitsStartOnce && visitsEndOnce

parseLine :: CaveMap -> String -> CaveMap
parseLine caveMap line = caveMap''
    where (startCave, '-':endCave) = break (=='-') line
          caveMap' = Map.insertWith (++) startCave [endCave] caveMap
          caveMap'' = Map.insertWith (++) endCave [startCave] caveMap'

parseCaveMap :: [String] -> CaveMap
parseCaveMap = foldl parseLine Map.empty

isSmall :: Cave -> Bool
isSmall = all isLower
