import Data.List
import Data.Maybe (catMaybes, isNothing)
import System.Environment (getArgs)

type Line = String
type InvalidChar = Char
type ExpectedChar = Char
type Score = Int

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"] -> part1
    ["part2"] -> part2
    _         -> usage

part1 :: IO ()
part1 = interact $ show . solve1 . lines

part2 :: IO ()
part2 = interact $ show . solve2 . lines

usage :: IO ()
usage = do
  putStrLn "Wrong parameters. Usage:"
  putStrLn "$ main <part1|part2>"

solve1 :: [Line] -> Int
solve1 = getSyntaxCheckerScore . getInvalidChars

solve2 :: [Line] -> Int
solve2 = getAutocompleteScore . getExpectedChars

getSyntaxCheckerScore :: [InvalidChar] -> Score
getSyntaxCheckerScore = sum . map toInvalidCharScore
  where toInvalidCharScore ')' = 3
        toInvalidCharScore ']' = 57
        toInvalidCharScore '}' = 1197
        toInvalidCharScore '>' = 25137

getAutocompleteScore :: [[ExpectedChar]] -> Int
getAutocompleteScore =  middle . sort . map toExpectedCharsScore
  where toExpectedCharScore ')' = 1
        toExpectedCharScore ']' = 2
        toExpectedCharScore '}' = 3
        toExpectedCharScore '>' = 4
        toExpectedCharsScore = foldl (\score char -> score * 5 + toExpectedCharScore char) 0 

getExpectedChars :: [Line] -> [[ExpectedChar]]
getExpectedChars = filter (not . null) . map fst . filter (isNothing . snd) . map (parseLine [])

getInvalidChars :: [Line] -> [InvalidChar]
getInvalidChars = catMaybes . map (snd . parseLine [])

parseLine :: [ExpectedChar] -> Line -> ([ExpectedChar], Maybe InvalidChar)
parseLine [] [] = ([], Nothing)
parseLine expectedChars [] = (expectedChars, Nothing)
parseLine [] (char:line)
  | isClosingParen char = ([], Just char)
  | otherwise = parseLine [closeParen char] line
parseLine (expectedChar:expectedChars) (char:line)
  | isClosingParen char && char == expectedChar = parseLine expectedChars line
  | isClosingParen char && char /= expectedChar = ((expectedChar:expectedChars), Just char)
  | otherwise = parseLine ((closeParen char):expectedChar:expectedChars) line

closeParen :: Char -> Char
closeParen '(' = ')'
closeParen '[' = ']'
closeParen '{' = '}'
closeParen '<' = '>'

isClosingParen :: Char -> Bool
isClosingParen = (`elem` ")]}>")

middle :: [a] -> a
middle [x] = x
middle [x,y] = x
middle xs = middle . init . tail $ xs
