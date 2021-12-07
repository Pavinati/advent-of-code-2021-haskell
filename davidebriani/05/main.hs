import Data.List
import System.Environment (getArgs)

type Point = (Int, Int)
type Segment = [Point]

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

solve1 :: String -> Int
solve1 input =
    let segments = filter (not . isOblique) . map parseSegment . lines $ input
        points = concat segments
        uniquePoints = nub points
        duplicatePoints = points \\ uniquePoints
    in length $ nub duplicatePoints

solve2 :: String -> Int
solve2 input =
    let segments = map parseSegment . lines $ input
        points = concat segments
        uniquePoints = nub points
        duplicatePoints = points \\ uniquePoints
    in length $ nub duplicatePoints

parseSegment :: String -> Segment
parseSegment line = interpolateSegment [point1, point2]
    where [pos1, _, pos2] = words line
          point1 = parsePoint pos1
          point2 = parsePoint pos2

parsePoint :: String -> Point
parsePoint string = (read x, read y)
    where (x, ',':y) = break (==',') string

interpolateSegment :: Segment -> Segment
interpolateSegment [point1, point2]
    | isHorizontal [point1, point2] = zip (range x1 x2) (repeat y1)
    | isVertical [point1, point2] = zip (repeat x1) (range y1 y2)
    | otherwise = zip (range x1 x2) (range y1 y2)
    where x1 = fst point1
          y1 = snd point1
          x2 = fst point2
          y2 = snd point2

range :: Int -> Int -> [Int]
range n1 n2 = if n1 <= n2 then [n1..n2] else [n1,(n1 - 1)..n2]

isHorizontal :: Segment -> Bool
isHorizontal segment = snd startPoint == snd endPoint
    where startPoint = head segment
          endPoint = last segment

isVertical :: Segment -> Bool
isVertical segment = fst startPoint == fst endPoint
    where startPoint = head segment
          endPoint = last segment

isOblique :: Segment -> Bool
isOblique segment = (not . isHorizontal $ segment) && (not . isVertical $ segment)
