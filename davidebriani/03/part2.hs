import Data.Function (on)
import Data.List (group, inits, isPrefixOf, sort, sortBy, transpose)

type Binary = String
type Bit = Char
type LifeSupportRating = Int

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

filterBinaries :: ([Binary] -> [Bit]) -> [Binary] -> Binary
filterBinaries chooseBits binaries = head . fst $ foldl filterFold (binaries, 0) binaries
    where filterFold = \(binaries, i) _ -> ((filter ((take i . chooseBits $ binaries) `isPrefixOf`) binaries), i + 1)

solve :: [Binary] -> LifeSupportRating
solve binaries = oxygenGeneratorRating * c02ScrubberRating
    where oxygenGeneratorRating = toDecimal $ filterBinaries mostCommonBits binaries
          c02ScrubberRating = toDecimal $ filterBinaries leastCommonBits binaries

main :: IO ()
main = interact $ show . solve . lines
