module Main where

-- Looots of replicated code, hopefully some alternative will be found
data DepthStatus = Increasing | NotIncreasing deriving (Eq)

depthsToStatuses :: Ord a => [a] -> [DepthStatus]
depthsToStatuses (fstDepth : depthList) = fst $ foldl compute ([], fstDepth) depthList
  where
    compute (acc, previousInt) currentInt = ((if currentInt > previousInt then Increasing else NotIncreasing) : acc, currentInt)

countIncreasing :: [DepthStatus] -> Int
countIncreasing = length . filter (== Increasing)

solve :: Ord a => [a] -> Int
solve = countIncreasing . depthsToStatuses

main =
  putStrLn . show . solve . values . map readInt . lines =<< readFile "input"
  where
    readInt x = read x :: Int
    values l = drop 2 $ zipWith3 (\x y z -> x + y + z) l (0 : l) (0 : 0 : l)