import System.Environment

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
  putStrLn "$  main part1"
  putStrLn "  or"
  putStrLn "$  main part2"

parseInt :: String -> Int
parseInt a = read a

part1 :: IO ()
part1 = do
  contents <- readFile "input"
  let depts = lines contents
      list  = map parseInt depts
      inc   = countIncreasing 0 list
  putStrLn $ show inc

countIncreasing :: Int -> [Int] -> Int
countIncreasing acc []  = acc
countIncreasing acc [x] = acc
countIncreasing acc (x:y:xs) = countIncreasing newAcc (y:xs)
                    where newAcc = if y > x then acc + 1 else acc

part2 :: IO ()
part2 = do
  contents <- readFile "input"
  let depts = lines contents
      list  = map parseInt depts
      inc   = countIncreasing2 0 list
  putStrLn $ show inc

countIncreasing2 :: Int -> [Int] -> Int
countIncreasing2 acc (a:b:c:d:xs) = countIncreasing2 newAcc (b:c:d:xs)
                    where newAcc = if b+c+d > a+b+c then acc + 1 else acc
countIncreasing2 acc _ = acc
