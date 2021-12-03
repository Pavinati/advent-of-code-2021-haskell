module Main where

import Data.Char
import Data.List
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

-- some utilities
binaryStringToDec :: String -> Int
binaryStringToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

binaryNot :: Char -> Char
binaryNot '0' = '1'
binaryNot '1' = '0'

-- why use space and $ when you can define your own combinators?
at :: (t1 -> t2) -> t1 -> t2
x `at` y = x y

on :: (a -> b) -> a -> b
(on) = ($)

-- now we roll
occurrencesOf :: Ord b => ([(Int, b)] -> (a, c)) -> [b] -> c
occurrencesOf cmp = snd . cmp . map (\g -> (length g, head g)) . group . sort

bitsAt :: (Eq a, Num a, Enum a) => a -> [[b]] -> [[(a, b)]]
bitsAt n = groupBy sameIndex . filter (\x -> fst x == n) . concat . map (zip [0, 1 ..])
  where
    sameIndex x y = fst x == fst y

get :: (Ord b, Num a1, Enum a1, Eq a1) => ([(Int, b)] -> (a2, c)) -> a1 -> [[b]] -> c
get cmp n = head . map (occurrencesOf cmp . dropIndex) . bitsAt n
  where
    dropIndex = map (\p -> snd p)

computeGamma :: (Int, String, [String]) -> (Int, String, [String])
computeGamma (n, acc, binaries) =
  if (length $ head binaries) == n
    then (n, reverse acc, binaries)
    else computeGamma (n + 1, mostCommonAtN : acc, binaries)
  where
    mostCommonAtN = get maximum n `on` binaries

solve :: [String] -> Int
solve xs = product gammaEpsilon
  where
    (_, gamma, _) = computeGamma (0, [], xs)
    gammaEpsilon = binaryStringToDec <$> [gamma, binaryNot <$> gamma]

part1 :: IO ()
part1 =
  putStrLn . show . solve . lines =<< readFile "input"

computeOxygenAndCo2 :: (Int, [String], [String]) -> (Int, [String], [String])
computeOxygenAndCo2 (n, oxygenList, co2List) =
  if (length (oxygenList) == length (co2List)) && (length (oxygenList) == 1)
    then (n, oxygenList, co2List)
    else computeOxygenAndCo2 (n + 1, newOxygen, newCo2)
  where
    mostOxygen = get maximum `at` n `on` oxygenList
    leastCo2 = get minimum `at` n `on` co2List
    newOxygen = filter (\l -> l !! n == mostOxygen) oxygenList
    newCo2 = filter (\l -> l !! n == leastCo2) co2List

solve' :: [String] -> Int
solve' xs = product lifesupport
  where
    (_, oxygenBinary, co2Binary) = computeOxygenAndCo2 (0, xs, xs)
    lifesupport = (binaryStringToDec . head) <$> [oxygenBinary, co2Binary]

part2 :: IO ()
part2 =
  putStrLn . show . solve' . lines =<< readFile "input"