countIncrements :: (Num a, Ord a) => [a] -> Int
countIncrements xs = length . filter (>0) $ zipWith (-) (tail xs) (init xs)

main :: IO ()
main = interact $ show . countIncrements . map read . lines
