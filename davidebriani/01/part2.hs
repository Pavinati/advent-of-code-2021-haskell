import Data.List (tails)

countIncrements :: (Num a, Ord a) => [a] -> Int
countIncrements xs = length . filter (>0) $ zipWith (-) (tail xs) (init xs)

windows :: Int -> [a] -> [[a]]
windows n = foldr (zipWith (:)) (repeat []) . take n . tails

sumWindows :: (Num a) => [[a]] -> [a]
sumWindows = map (foldr (+) 0)

main :: IO ()
main = interact $ show . countIncrements . sumWindows . windows 3 . map read . lines
