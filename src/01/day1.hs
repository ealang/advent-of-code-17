import Data.Char
import Text.Printf

zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex list = list `zip` [0..]

elemsAreEqual :: [Int] -> Int -> Int -> Bool
elemsAreEqual list step i =
  let n = length list
  in list !! i == list !! ((i + step) `mod` n)

day1 :: [Int] -> Int -> Int
day1 list step =
  let elemFilter = elemsAreEqual list step
  in sum [ v | (v, i) <- zipWithIndex list, elemFilter i]

main = do
  input <- readFile "day1-input.txt"
  let numbers = map digitToInt input
  printf "part1: %d\n" $ day1 numbers 1
  printf "part2: %d\n" $ day1 numbers $ length numbers `quot` 2
