import System.Environment
import Data.Char (digitToInt)
import Data.List (transpose)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"] -> part1
    ["part2"] -> part2
    _         -> usage

usage :: IO ()
usage = do
  putStrLn "Wrong parameters."
  putStrLn "usage:"
  putStrLn "$  main <part1|part2>"

incrementWhen :: (a -> Bool) -> a -> Int -> Int
incrementWhen f v acc = if f v then acc + 1 else acc

stringXor :: [Char] -> [Char]
stringXor bits = map (xor) bits
  where xor c = if c == '0' then '1' else '0'

toDec :: String -> Int
toDec binary = foldl sumExp 0 binary
  where sumExp acc c = acc * 2 + digitToInt c

countBits :: [Char] -> (Int, Int)
countBits bits =
  ( foldr (incrementWhen (=='0')) 0 bits
  , foldr (incrementWhen (=='1')) 0 bits
  )

gamma :: [(Int, Int)] -> String
gamma list = foldl convert "" list
  where convert s (a,b) = s ++ (if a > b then "0" else "1")

solve :: String -> Int
solve input =
  let bitCount = map countBits $ transpose $ lines input
      gammaRate = gamma bitCount
      epsilonRate = stringXor gammaRate
  in (toDec gammaRate) * (toDec epsilonRate)

part1 :: IO ()
part1 = interact $ show . solve

filterByBit :: Char -> Int -> String -> Bool
filterByBit bit index number = (number !! index) == bit

oxygenGeneratorRating :: [String] -> Int -> String
oxygenGeneratorRating [number] _    = number
oxygenGeneratorRating numbers index =
  let (zeros, ones) = countBits $ map (!! index) numbers
      filterChar = if ones >= zeros then '1' else '0'
      filteredNumbers = filter (filterByBit filterChar index) numbers
  in oxygenGeneratorRating filteredNumbers (index + 1)

co2ScrubberRating :: [String] -> Int -> String
co2ScrubberRating [number] _ = number
co2ScrubberRating numbers index =
  let (zeros, ones) = countBits $ map (!! index) numbers
      filterChar = if zeros <= ones then '0' else '1'
      filteredNumbers = filter (filterByBit filterChar index) numbers
  in co2ScrubberRating filteredNumbers (index + 1)

solve' :: String -> Int
solve' input =
  let numbers = lines input
      o = toDec $ oxygenGeneratorRating numbers 0
      c = toDec $ co2ScrubberRating numbers 0
  in o * c

part2 :: IO ()
part2 = interact $ show . solve'

