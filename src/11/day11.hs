import Data.List.Split (splitOn)

data Direction = N | NE | SE | S | SW | NW | HOLD deriving (Show)

type Coord = (Int, Int)

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

followDirections :: Coord -> [Direction] -> Coord
followDirections = foldl move

dirToOrigin :: Coord -> Direction
dirToOrigin (x, y)
  | x == 0 && y == 0 = HOLD
  | x > 0 && y > 0   = SW
  | x > 0 && y < 0   = NW
  | x < 0 && y > 0   = SE
  | x < 0 && y < 0   = NE
  | y > 0            = S
  | y < 0            = N
  | x > 0            = NW

distFromOrigin :: Coord -> Int
distFromOrigin fromPos = length $ takeWhile (/= (0, 0)) positions
  where positions = iterate moveToOrigin fromPos
        moveToOrigin pos = move pos (dirToOrigin pos)
        
part1 :: [Direction] -> Int
part1 directions = distFromOrigin $ followDirections (0, 0) directions

main = do
  directions <- map parseDirection .
                splitOn "," .
                takeWhile (/= '\n') <$> readFile "input.txt"

  print $ part1 directions
