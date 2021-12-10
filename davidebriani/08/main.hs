import Data.List
import Data.Function (on)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

type Digit = Char
type Segment = Char
type Pattern = [Segment]

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
solve1 = length . filter (`elem` "1478") . concat . map decodeOutput . map parseLine . lines

solve2 :: String -> Int
solve2 = sum . map read . map decodeOutput . map parseLine . lines

decodeOutput :: ([Pattern], [Pattern]) -> String
decodeOutput (patterns, output) = fromJust . flip lookup patternMap <$> output
  where patternMap = decodePatterns patterns

decodePatterns :: [Pattern] -> [(Pattern, Digit)]
decodePatterns patterns = patternMap
  where _1 = (patterns !! 0) -- 1 is the only pattern with length 2 (i.e. 2 segments), the lowest
        _7 = (patterns !! 1) -- 7 is the only pattern with length 3
        _4 = (patterns !! 2) -- 4 is the only pattern with length 4
        _8 = (patterns !! 9) -- 8 is the only pattern with length 7, the highest
        -- 2 is the pattern with length 5 that summed with the 4 pattern gives the 8 pattern
        _2 = maximumBy (compare `on` (length . nub . (++ _4))) . filter ((==5) . length) $ patterns
        -- 3 is the pattern with length 5 that diffed with the 1 pattern has length 3
        _3 = fromJust . find ((==3) . length . (\\ _1)) . filter ((==5) . length) $ patterns
        -- 5 is the remaining pattern with length 5
        _5 = head . filter (`notElem` [_2, _3]) . filter ((==5) . length) $ patterns
        -- 9 is the pattern with length 6 that summed with the 4 pattern has length 6
        _9 = fromJust . find ((==6) . length . nub . (++ _4)) . filter ((==6) . length) $ patterns
        -- 0 is the pattern with length 6 that summed with 3-1 has length 7
        _0 = fromJust . find ((==7) . length . nub . (++ (_3 \\ _1))) . filter ((==6) . length) $ patterns
        -- 6 is the remaining pattern with length 6
        _6 = head . filter (`notElem` [_9, _0]) . filter ((==6) . length) $ patterns
        patternMap = [(_0, '0'), (_1, '1'), (_2, '2'), (_3, '3'), (_4, '4'), (_5, '5'), (_6, '6'), (_7,'7'), (_8, '8'), (_9, '9')]

parseLine :: String -> ([Pattern], [Pattern])
parseLine line = (patterns, output)
  where lineParts = groupBy ((==) `on` (=='|')) line
        patterns = sortBy (compare `on` length) . map sort . words . head $ lineParts
        output = map sort . words . last $ lineParts
