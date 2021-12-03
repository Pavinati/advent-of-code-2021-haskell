type Aim = Int
type Depth = Int
type Position = Int

sumTrajectory :: (Aim, Depth, Position) -> [String] -> (Aim, Depth, Position)
sumTrajectory (a, d, p) ("up":n:trajectory) = sumTrajectory (a - (read n), d, p) trajectory
sumTrajectory (a, d, p) ("down":n:trajectory) = sumTrajectory (a + (read n), d, p) trajectory
sumTrajectory (a, d, p) ("forward":n:trajectory) = sumTrajectory (a, d  + (read n) * a, p + (read n)) trajectory
sumTrajectory (a, d, p) _ = (a, d, p)

product :: (Aim, Depth, Position) -> Int
product (a, d, p) = d * p

main :: IO ()
main = interact $ show . product . sumTrajectory (0, 0, 0) . words
