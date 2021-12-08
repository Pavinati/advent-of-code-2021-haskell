import Prelude hiding (lookup)
import Data.Map (Map, empty, fold, insert, lookup)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test1"] -> test1
    ["part1"] -> part1
    ["test2"] -> test2
    ["part2"] -> part2
    _         -> usage

usage :: IO ()
usage = do
  putStrLn "Wrong parameters."
  putStrLn "usage:"
  putStrLn "$  main <argument>"
  putStrLn "<argument> can be any of: test1, part1, test2, part2"

type Point = (Int, Int)
type Line = (Point, Point)
type PointMap = Map Point Int

isHorizontal :: Line -> Bool
isHorizontal ((x1, _), (x2, _)) = x1 == x2

isVertical :: Line -> Bool
isVertical ((_, y1), (_, y2)) = y1 == y2

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen c l =
  case dropWhile c l of
    [] -> []
    l' -> let (x,xs) = break c l'
          in x : splitWhen c xs

parsePoint :: String -> Point
parsePoint s = let [x,y] = splitWhen (==',') s
               in (read x, read y)

parseLine :: String -> Line
parseLine s = let [p1,_,p2] = splitWhen (==' ') s
            in (parsePoint p1, parsePoint p2)

getInput :: [String] -> [Line]
getInput = map parseLine

linePoints :: Line -> [Point]
linePoints ((x1, y1), (x2, y2))
  | x1 == x2  = [(x1, y) | y <- range y1 y2]
  | y1 == y2  = [(x, y1) | x <- range x1 x2]
  | otherwise = []
  where range a b = if a > b then [b..a] else [a..b]

incrementPointInMap :: Point -> PointMap -> PointMap
incrementPointInMap p m =
  case lookup p m of
  Just x -> insert p (x+1) m
  Nothing -> insert p 1 m

countGreaterThen2 :: Int -> Int -> Int
countGreaterThen2 v acc = if v > 1 then acc + 1 else acc

solve :: [Line] -> Int
solve = fold countGreaterThen2 0 . foldr incrementPointInMap (empty) . concat . map linePoints

test1 :: IO ()
test1 = do
  putStrLn . show . solve . getInput . lines =<< readFile "test"

part1 :: IO ()
part1 = do
  putStrLn . show . solve . getInput . lines =<< readFile "input"

isDiagonal :: Line -> Bool
isDiagonal ((x1, y1), (x2, y2)) =
  abs (x1-x2) == abs (y1-y2)

diagonalLine :: Point -> Point -> [Point]
diagonalLine (x1, y1) (x2, y2) =
  let dx = signum (x2-x1)
      dy = signum (y2-y1)
      length = abs (x1 - x2)
  in [(x1 + (i*dx), y1 + (i*dy)) | i <- [0..length]]

linePoints' :: Line -> [Point]
linePoints' line
  | x1 == x2        = [(x1, y) | y <- range y1 y2]
  | y1 == y2        = [(x, y1) | x <- range x1 x2]
  | isDiagonal line = diagonalLine (x1, y1) (x2, y2)
  where range a b = if a > b then [b..a] else [a..b]
        ((x1, y1), (x2, y2)) = line

solve' :: [Line] -> Int
solve' = fold countGreaterThen2 0 . foldr incrementPointInMap (empty) . concat . map linePoints'

test2 :: IO ()
test2 = do
  putStrLn . show . solve' . getInput . lines =<< readFile "test"

part2 :: IO ()
part2 = do
  putStrLn . show . solve' . getInput . lines =<< readFile "input"

