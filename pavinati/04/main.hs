import System.Environment
import Data.List (transpose)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test1"] -> test1
    ["part1"] -> part1
    ["test2"] -> test2
    ["part2"] -> part2
    _         -> usage


usage :: IO ()
usage = do
  putStrLn "Wrong parameters."
  putStrLn "usage:"
  putStrLn "$  main <argument>"
  putStrLn "<argument> can be any of: test1, part1, part2"


type Cell = (Int, Bool)
type Board = [[Cell]]


parseInt :: String -> Int
parseInt = read


parseBoards :: [String] -> [Board]
parseBoards = map parseBoard . splitWhen (=="")
  where parseBoard = (map.map) initBoardCell . map (splitWhen (==' '))


getInput :: [String] -> ([Int], [Board])
getInput (stringNumbers:stringBoards) =
  let numbers = map parseInt $ splitWhen (==',') stringNumbers
      boards = parseBoards stringBoards
  in (numbers, boards)


splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen c l =
  case dropWhile c l of
    [] -> []
    l' -> let (x,xs) = break c l'
          in x : splitWhen c xs


findFirst :: [Maybe a] -> Maybe a
findFirst []           = Nothing
findFirst (Nothing:xs) = findFirst xs
findFirst (Just x:_)   = Just x


initBoardCell :: String -> Cell
initBoardCell s = (parseInt s, False)


applyNumber :: Int -> [Board] -> [Board]
applyNumber number = (map.map.map) (setIfNumber number)
  where setIfNumber n (a, v) | a == n   = (a, True)
                             | otherwise = (a, v)


isWinningBoard :: Board -> Bool
isWinningBoard board =
  let winningByRows = or $ map isWinningRow board
      winningByCols = or $ map isWinningRow $ transpose board
  in winningByRows || winningByCols
  where isWinningRow = all snd


getWinningBoard :: Board -> Maybe Board
getWinningBoard board =
  if isWinningBoard board
  then Just board
  else Nothing


check :: [Board] -> Maybe Board
check = findFirst . map getWinningBoard


play :: [Int] -> [Board] -> Maybe (Int, Board)
play [] state     = Nothing
play (x:xs) state =
  let newState = applyNumber x state
  in case check newState of
    Nothing      -> play xs newState
    Just winning -> Just (x, winning)


sumUnchecked :: Board -> Int
sumUnchecked = sum . map unchecked . concat
  where unchecked c = case c of
                      (a, False) -> a
                      (_, True)  -> 0


solve :: ([Int], [Board]) -> Maybe Int
solve (numbers, boards) =
  case play numbers boards of
    Just (n, winning) -> Just (n * sumUnchecked winning)
    Nothing           -> Nothing


test1 :: IO ()
test1 = do
  putStrLn . show . solve . getInput . lines =<< readFile "test"


part1 :: IO ()
part1 = do
  putStrLn . show . solve . getInput . lines =<< readFile "input"


checkLastWinning :: [Board] -> [Board] -> Maybe Board
checkLastWinning g1 g2 =
  case filter (not . isWinningBoard . fst) $ zip g1 g2 of
  [(_, lastWinning)] -> Just lastWinning
  _                  -> Nothing


playDumb :: [Int] -> [Board] -> Maybe (Int, Board)
playDumb [] state     = Nothing
playDumb (x:xs) state =
  let newState = applyNumber x state
  in case checkLastWinning state newState of
    Nothing          -> playDumb xs newState
    Just lastWinning -> Just (x, lastWinning)


solve2 :: ([Int], [Board]) -> Maybe Int
solve2 (numbers, boards) =
  case playDumb numbers boards of
    Just (n, winning) -> Just (n * sumUnchecked winning)
    Nothing           -> Nothing


test2 :: IO ()
test2 = do
  putStrLn . show . solve2 . getInput . lines =<< readFile "test"


part2 :: IO ()
part2 = do
  putStrLn . show . solve2 . getInput . lines =<< readFile "input"

