import Text.Printf
import qualified Data.Map as Map
import Data.List (find)

cellPositions :: [(Int, Int)]
cellPositions = fromShell 0
  where fromShell 0 = (0, 0): fromShell 1
        fromShell shell = [(shell, y) | y <- [(1 - shell) .. shell]] ++
                          [(x, shell) | x <- reverse [-shell .. (shell - 1)]] ++
                          [(-shell, y) | y <- reverse [-shell .. (shell - 1)]] ++
                          [(x, -shell) | x <- [(1 - shell) .. shell]] ++
                          fromShell (shell + 1)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [ (x + dx, y + dy) |
                     dx <- [-1..1], dy <- [-1..1],
                     dx /= 0 || dy /= 0 ]

cellSums :: [Int]
cellSums = map snd $ scanl nextSum (initCache, 1) (drop 1 cellPositions)
  where nextSum (cache, _) pos = let
          cellVal = sum $ map (\p -> Map.findWithDefault 0 p cache)
                              (neighbors pos)
          in (Map.insert pos cellVal cache, cellVal)
        initCache = Map.fromList [((0, 0), 1)]

day3Part1 :: Int -> Int
day3Part1 num = abs x  + abs y
  where (x, y) = cellPositions !! (num - 1)

day3Part2 :: Int -> Maybe Int
day3Part2 targetVal = find (>targetVal) cellSums

main = do
  let input = 277678
  putStrLn $ "part1: " ++ show (day3Part1 input)
  putStrLn $ "part2: " ++ show (day3Part2 input)
