module Main where

import Data.Char (digitToInt)
import Data.List
import Data.Vector (Vector, fromList, imap, (!), (!?))
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1", input] -> part1 input
    ["part2", input] -> part2 input

type Matrix a = Vector (Vector a)

type Map = Matrix Int

parse :: String -> Map
parse s = fromList . map fromList . map (map digitToInt) $ lines s

findLows :: Map -> [(Int, Int)]
findLows m = [(x, y) | x <- [0 .. maxRow], y <- [0 .. maxColumn], isLow (x, y) m]
  where
    maxRow = length m -1
    maxColumn = length (m ! 0) -1

toValue :: Matrix a -> (Int, Int) -> a
toValue m (x, y) = m ! x ! y --indexing is safe, since all points we will use are in the map

isLow :: Ord a => (Int, Int) -> Matrix a -> Bool
isLow p m = all (> toValue m p) (toValue m <$> neighbours p m)

neighbours :: Foldable t => (Int, Int) -> Vector (t a) -> [(Int, Int)]
neighbours (x, y) m = filter checkBounds [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]
  where
    checkBounds (x, y)
      | x < 0 || x > length m -1 || y < 0 || y > length (m ! 0) -1 = False
      | otherwise = True

part1 :: FilePath -> IO ()
part1 input = putStrLn . show . sum . computePointRisk . parse =<< readFile input
  where
    computePointRisk m = map ((+ 1) . toValue m) $ findLows m

visitPoint :: (Matrix Bool, Matrix Int, [(Int, Int)]) -> (Int, Int) -> (Matrix Bool, Matrix Int, [(Int, Int)])
visitPoint (visited, m, known) p =
  let (visited', more) = runVisit m visited p in (visited', m, more ++ known)
  where
    runVisit m visited p = if check p then mkBasin m visited p else (visited, [])
    check (x, y) = m ! x ! y < 9 && not (visited ! x ! y)

mkBasin :: Map -> Matrix Bool -> (Int, Int) -> (Matrix Bool, [(Int, Int)])
mkBasin m visited p@(x, y) =
  let visited' = imap (\i row -> (imap (\j _ -> if i == x && j == y then True else visited ! i ! j)) row) visited
      (visited'', _, basin) = foldl visitPoint (visited', m, [p]) (neighbours p m)
   in (visited'', basin)

part2 :: FilePath -> IO ()
part2 input = do
  f <- readFile input
  let m = parse f
  let visited = imap (\_ row -> (imap (\_ _ -> False)) row) m
  let basins = map (snd . mkBasin m visited) . findLows $ m
  putStrLn . show . product . take 3 . reverse . sort . map length $ basins
