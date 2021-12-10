module Main where

import Data.List (sort)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1", input] -> part1 input
    ["part2", input] -> part2 input

data Chunck = Legal | Incomplete [Char] | Corrupted Char

parse :: [Char] -> [Char] -> Chunck
parse [] [] = Legal
parse open [] = Incomplete open
parse open ('(' : cs) = parse ('(' : open) cs
parse open ('[' : cs) = parse ('[' : open) cs
parse open ('{' : cs) = parse ('{' : open) cs
parse open ('<' : cs) = parse ('<' : open) cs
parse (o : os) (')' : cs) = if o == '(' then parse os cs else Corrupted ')'
parse (o : os) (']' : cs) = if o == '[' then parse os cs else Corrupted ']'
parse (o : os) ('}' : cs) = if o == '{' then parse os cs else Corrupted '}'
parse (o : os) ('>' : cs) = if o == '<' then parse os cs else Corrupted '>'

part1 :: FilePath -> IO ()
part1 input = putStrLn . show . sum . map score . filter isCorrupted . map (parse []) . lines =<< readFile input
  where
    isCorrupted (Corrupted _) = True
    isCorrupted _ = False
    score (Corrupted ')') = 3
    score (Corrupted ']') = 57
    score (Corrupted '}') = 1197
    score (Corrupted '>') = 25137

middle :: [a] -> a -- actually unsafe on empty lists, but AoC assures no list will be empty
middle xs = head . drop mid $ xs
  where
    mid = floor . (/ 2) . fromIntegral . length $ xs

part2 :: FilePath -> IO ()
part2 input = putStrLn . show . middle . sort . map score . filter isIncomplete . map (parse []) . lines =<< readFile input
  where
    isIncomplete (Incomplete _) = True
    isIncomplete _ = False
    score (Incomplete cs) = foldl autocompleteScore 0 cs
    autocompleteScore acc c =
      acc * 5
        + case c of
          '(' -> 1
          '[' -> 2
          '{' -> 3
          '<' -> 4
