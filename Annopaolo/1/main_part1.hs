module Main where

data DepthStatus = Increasing | NotIncreasing deriving (Eq)

depthsToStatuses :: Ord a => [a] -> [DepthStatus]
depthsToStatuses (fstDepth : depthList) = fst $ foldl compute ([], fstDepth) depthList
  where
    compute (acc, previousInt) currentInt = ((if currentInt > previousInt then Increasing else NotIncreasing) : acc, currentInt)

countIncreasing :: [DepthStatus] -> Int
countIncreasing = length . filter (== Increasing)

solve :: Ord a => [a] -> Int
solve = countIncreasing . depthsToStatuses

main :: IO ()
main =
  putStrLn . show . solve . map readInt . lines =<< readFile "input"
  where
    readInt x = read x :: Int
