import Text.Printf

numToPos :: Int -> (Int, Int)
numToPos 1 = (0, 0)
numToPos num = fromShell 1 (num - 2)
  where fromShell curShell i
         | i <= tr =
            (curShell, i - middle)
         | i <= tl =
            (middle - (i - tr - 1), curShell)
         | i <= bl =
            (-curShell, middle - (i - tl - 1))
         | i <= br =
            ((i - bl - 1) - middle, -curShell)
         | otherwise =
            fromShell (curShell + 1) (i - br - 1)
         where middle = curShell - 1
               sideLen = curShell * 2
               tr = sideLen - 1
               tl = tr + sideLen
               bl = tl + sideLen
               br = bl + sideLen

day3 :: Int -> Int
day3 num = abs x  + abs y 
   where (x, y) = numToPos num

main = do
  let input = 277678
  printf "part1: %d\n" $ day3 input

