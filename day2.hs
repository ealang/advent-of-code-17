import Text.Printf

rowDiff :: [Int] -> Int
rowDiff row = abs(minimum row  - maximum row)

evenDiv :: [Int] -> Int
evenDiv = head . divisors
  where divisors list = [ i `div` j |
                          i <- list, j <- list,
                          i /= j, i `mod` j == 0 ]

day2 :: ([Int] -> Int) -> [[Int]] -> Int
day2 rowCSum = sum . map rowCSum

main = do
  input <- readFile "day2-input.txt"
  let numbers = [[read word::Int | word <- words line] |
                   line <- lines input]
  printf "part1: %d\n" $ day2 rowDiff numbers
  printf "part2: %d\n" $ day2 evenDiv numbers

