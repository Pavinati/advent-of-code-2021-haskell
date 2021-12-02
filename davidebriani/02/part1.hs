type Depth = Int
type Position = Int

sumTrajectory :: (Depth, Position) -> [String] -> (Depth, Position)
sumTrajectory (d, p) ("up":n:trajectory) = sumTrajectory (d - (read n), p) trajectory
sumTrajectory (d, p) ("down":n:trajectory) = sumTrajectory (d + (read n), p) trajectory
sumTrajectory (d, p) ("forward":n:trajectory) = sumTrajectory (d, p + (read n)) trajectory
sumTrajectory (d, p) _ = (d, p)

product :: (Depth, Position) -> Int
product (d, p) = d * p

main :: IO ()
main = interact $ show . product . sumTrajectory (0, 0) . words
