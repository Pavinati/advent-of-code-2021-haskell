import Data.List (delete, find, transpose)
import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)

data Board = Board { rows :: [[Int]] } deriving (Eq)

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
    let (numbersToCall, boards) = parseInput input
        (calledNumbers, _, winningBoard) = get_first_bingo [] numbersToCall boards
    in boardFinalScore winningBoard calledNumbers

solve2 :: String -> Int
solve2 input =
    let (numbersToCall, boards) = parseInput input
        (calledNumbers, _, winningBoard) = get_last_bingo [] numbersToCall boards
    in boardFinalScore winningBoard calledNumbers

get_first_bingo :: [Int] -> [Int] -> [Board] -> ([Int], [Int], Board)
get_first_bingo calledNumbers numbersToCall boards
    | any (`winsWith` calledNumbers) boards = (calledNumbers, numbersToCall, winningBoard)
    | otherwise = get_first_bingo (calledNumbers ++ [head numbersToCall]) (tail numbersToCall) boards
    where winningBoard = fromJust $ find (`winsWith` calledNumbers) boards

get_last_bingo :: [Int] -> [Int] -> [Board] -> ([Int], [Int], Board)
get_last_bingo calledNumbers numbersToCall [board] = get_first_bingo calledNumbers numbersToCall [board]
get_last_bingo calledNumbers numbersToCall boards =
    let (updatedCalledNumbers, updatedNumbersToCall, winningBoard) = get_first_bingo calledNumbers numbersToCall boards
        losingBoards = delete winningBoard boards
    in get_last_bingo updatedCalledNumbers updatedNumbersToCall losingBoards

winsWith :: Board -> [Int] -> Bool
board `winsWith` calledNumbers = any (all isCalledNumber) boardRows || any (all isCalledNumber) boardColumns
    where boardRows = rows board
          boardColumns = transpose boardRows
          isCalledNumber = (`elem` calledNumbers)

boardFinalScore :: Board -> [Int] -> Int
boardFinalScore winningBoard calledNumbers =
    let winningNumber = last calledNumbers
        boardNumbers = concat . rows $ winningBoard
        unmarkedNumbers = filter (`notElem` calledNumbers) boardNumbers
        unmarkedNumbersSum = foldl (+) 0 unmarkedNumbers
        finalScore = unmarkedNumbersSum * winningNumber
    in finalScore

parseInput :: String -> ([Int], [Board])
parseInput input = (numbersToCall, boards)
    where nonEmptyLines = filter (not . null) . lines $ input
          numbersToCall = parseNumbersToCall $ head nonEmptyLines
          boards = parseBoards $ drop 1 nonEmptyLines

parseNumbersToCall :: String -> [Int]
parseNumbersToCall = map read . splitOn ','

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards lines = (Board { rows = lineToBoardRow <$> boardLines }) : (parseBoards otherLines)
    where boardLines = take 5 lines
          otherLines = drop 5 lines

lineToBoardRow :: String -> [Int]
lineToBoardRow = map read . words

splitOn :: Char -> String -> [String]
splitOn delimiter string =
    case break (==delimiter) string of
        (init, delimiter:tail) -> init : splitOn delimiter tail
        (init, "")             -> [init]
