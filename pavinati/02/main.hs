import System.Environment

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


data Instruction
  = Forward Int
  | Down Int
  | Up Int

type Position = (Int, Int)


parseInstruction :: String -> Instruction
parseInstruction s = case words s of
  [i, q] -> let quantity = read q
            in case i of
              "forward" -> Forward quantity
              "down"    -> Down quantity
              "up"      -> Up quantity


executeInstruction :: Instruction -> Position -> Position
executeInstruction instruction (x,y) =
  case instruction of
    Forward q -> (x+q, y)
    Down q    -> (x, y+q)
    Up q      -> (x, y-q)


test1 :: IO ()
test1 = do
  let instructions =
        [ Forward 5
        , Down 5
        , Forward 8
        , Up 3
        , Down 8
        , Forward 2
        ]
      (h, d) = foldr executeInstruction (0, 0) instructions
  putStrLn $ show $ h * d == 150


part1 :: IO ()
part1 = do
  contents <- readFile "input"
  let instructions = map parseInstruction $ lines contents
      (h, d) = foldr executeInstruction (0, 0) instructions
  putStrLn $ show (h * d)


type DirectedPosition = (Int, Int, Int)


executeInstruction2 :: DirectedPosition -> Instruction -> DirectedPosition
executeInstruction2 (x,y,a) instruction =
  case instruction of
    Forward q -> (x+q, y+q*a, a)
    Down q    -> (x, y, a+q)
    Up q      -> (x, y, a-q)


test2 :: IO ()
test2 = do
  let instructions =
        [ Forward 5
        , Down 5
        , Forward 8
        , Up 3
        , Down 8
        , Forward 2
        ]
      (h, d, a) = foldl executeInstruction2 (0, 0, 0) instructions
  putStrLn $ show (h, d, a)
  putStrLn $ show $ h * d == 900


part2 :: IO ()
part2 = do
  contents <- readFile "input"
  let instructions = map parseInstruction $ lines contents
      (h, d, _) = foldl executeInstruction2 (0, 0, 0) instructions
  putStrLn $ show (h, d)
  putStrLn $ show (h * d)

