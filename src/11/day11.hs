import Data.List.Split (splitOn)

type Coord = (Int, Int)

data Direction = N | NE | SE | S | SW | NW deriving (Show)

parseDirection :: String -> Direction
parseDirection "n" = N
parseDirection "ne" = NE
parseDirection "se" = SE
parseDirection "s" = S
parseDirection "sw" = SW
parseDirection "nw" = NW

move :: Coord -> Direction -> Coord
move (x, y) N = (x, y + 2)
move (x, y) NE = (x + 1, y + 1)
move (x, y) SE = (x + 1, y - 1)
move (x, y) S = (x, y - 2)
move (x, y) SW = (x - 1, y - 1)
move (x, y) NW = (x - 1, y + 1)

pathFrom :: Coord -> [Direction] -> [Coord]
pathFrom = scanl move

distFromOrigin :: Coord -> Int
distFromOrigin (x, y) = diag + dx + dy
  where diag = min (abs x) (abs y)
        dx = abs x - diag
        dy = (abs y - diag) `div` 2

part1 :: [Direction] -> Int
part1 = distFromOrigin . last . pathFrom (0, 0)

part2 :: [Direction] -> Int
part2 directions = maximum $ map distFromOrigin (pathFrom (0, 0) directions)

main = do
  directions <- map parseDirection .
                splitOn "," .
                takeWhile (/= '\n') <$> readFile "input.txt"
  print $ part1 directions
  print $ part2 directions
