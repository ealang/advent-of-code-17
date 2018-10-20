import qualified Data.Map as Map

type Jumps = Map.Map Int Int

walkFrom :: (Int -> Int) -> Jumps -> Int -> Int -> Int
walkFrom updateRule jumps pos count =
  if nextPos >= length jumps then count + 1
  else walkFrom updateRule nextJumps nextPos $! (count + 1)
  where jump = jumps Map.! pos
        nextPos = pos + jump
        nextJumps = Map.insert pos (updateRule jump) jumps

problem1 :: Jumps -> Int
problem1 jumps = walkFrom updateRule jumps startPos startCount
  where updateRule = (+1)
        startPos = 0
        startCount = 0

problem2 :: Jumps -> Int
problem2 jumps = walkFrom updateRule jumps startPos startCount
  where updateRule j = if j >= 3 then j - 1 else j + 1
        startPos = 0
        startCount = 0

main = do
  input <- readFile "day5-input.txt"
  let numbers = [read line::Int | line <- lines input]
      jumps = Map.fromList $ [0..] `zip` numbers 
  print $ problem1 jumps
  print $ problem2 jumps

