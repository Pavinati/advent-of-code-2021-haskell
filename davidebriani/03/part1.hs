import Data.Function (on)
import Data.List (group, sort, sortBy, transpose)

type Binary = String
type Bit = Char
type PowerConsumption = Int

toDecimal :: Binary -> Int
toDecimal = foldl (\acc x -> acc * 2 + read [x]) 0

mostCommonElement :: (Ord a, Eq a) => [a] -> a
mostCommonElement = head . last . sortBy (compare `on` length) . group . sort

leastCommonElement :: (Ord a, Eq a) => [a] -> a
leastCommonElement = head . head . sortBy (compare `on` length) . group . sort

mostCommonBits :: [Binary] -> [Bit]
mostCommonBits = map mostCommonElement . transpose

leastCommonBits :: [Binary] -> [Bit]
leastCommonBits = map leastCommonElement . transpose

solve :: [Binary] -> PowerConsumption
solve binaries = gammaRate * epsilonRate
    where gammaRate = toDecimal $ mostCommonBits binaries
          epsilonRate = toDecimal $ leastCommonBits binaries

main :: IO ()
main = interact $ show . solve . lines
