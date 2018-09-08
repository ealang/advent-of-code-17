import Text.Printf
import qualified Data.Map as Map
import Data.List (find)

numToPos :: Int -> (Int, Int)
numToPos 1 = (0, 0)
numToPos num = fromShell 1 (num - 2)
  where fromShell curShell i
         | i < tr =
            (curShell, i - middle)
         | i < tl =
            (middle - (i - tr), curShell)
         | i < bl =
            (-curShell, middle - (i - tl))
         | i < br =
            ((i - bl) - middle, -curShell)
         | otherwise =
            fromShell (curShell + 1) (i - br)
         where middle = curShell - 1
               sideLen = curShell * 2
               tr = sideLen
               tl = tr + sideLen
               bl = tl + sideLen
               br = bl + sideLen

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [ (x - 1, y), (x + 1, y),
                     (x, y - 1), (x, y + 1),
                     (x - 1, y - 1), (x - 1, y + 1),
                     (x + 1, y + 1), (x + 1, y - 1) ]

day3Part1 :: Int -> Int
day3Part1 num = abs x  + abs y
  where (x, y) = numToPos num

day3Part2 :: Int -> Maybe Int
day3Part2 targetVal = find (>targetVal) cellValues
  where fReduce (cache, _) curNum = let
          cellPos = numToPos curNum
          positions = neighbors cellPos
          valAtPos pos = Map.findWithDefault 0 pos cache
          cellVal = sum $ map valAtPos positions
          in (Map.insert cellPos cellVal cache, cellVal)
        initCache = Map.fromList [((0, 0), 1)]
        cellValues = map snd $ scanl fReduce (initCache, 1) [2..]

main = do
  let input = 277678
  putStrLn $ "part1: " ++ show (day3Part1 input)
  putStrLn $ "part2: " ++ show (day3Part2 input)

