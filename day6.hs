import qualified Data.Map as Map
import qualified Data.List as List

type Memory = [Int]

maxIndex :: Ord a => [a] -> (a, Int)
maxIndex list = maxIndex' (tail . withIndex $ list) (head list) 0
  where maxIndex' [] bestV bestI = (bestV, bestI)
        maxIndex' ((v, i):xs) bestV bestI
          | v > bestV = maxIndex' xs v i
          | otherwise = maxIndex' xs bestV bestI

withIndex :: [a] -> [(a, Int)]
withIndex a = a `zip` [0..]

distribute :: Memory -> Memory
distribute memory = map bankVal (withIndex memory)
  where (maxV, maxI) = maxIndex memory
        n = length memory
        transpose i = (i + n - maxI - 1) `mod` n
        remainder i = if transpose i < (maxV `mod` n) then 1 else 0
        original v i = if i == maxI then 0 else v
        bankVal (v, i) =
          (maxV `div` n) + remainder i + original v i

distributeWithHistory :: Memory -> [(Map.Map Memory Int, Memory)]
distributeWithHistory memory =
  scanl
    next
    initial
    (withIndex . tail . iterate distribute $ memory)
  where next (history, prev) (cur, i) = (Map.insert prev i history, cur)
        initial = (Map.empty, memory)

part1 :: Memory -> Int
part1 memory = length $
  takeWhile
    (\(seen, mem) -> not $ Map.member mem seen)
    (distributeWithHistory memory)

part2 :: Memory -> Int
part2 memory =
  let Just ((seen, mem), curIndex) =
        List.find (\((seen, mem), i) -> Map.member mem seen)
                  (withIndex . distributeWithHistory $ memory)
      firstIndex = seen Map.! mem
  in curIndex - firstIndex

main = do
  input <- readFile "day6-input.txt"
  let memory = [read s::Int | s <- words . head . lines $ input]
  putStrLn $ "part1: " ++ show (part1 memory)
  putStrLn $ "part2: " ++ show (part2 memory)
