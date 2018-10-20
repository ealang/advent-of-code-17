import qualified Data.List as List

policy1 :: Eq a => [a] -> Bool
policy1 elems = length elems == length (List.nub elems)

policy2 :: Ord a => [[a]] -> Bool
policy2 phrases = policy1 $ map List.sort phrases

countValid :: ([String] -> Bool) -> [String] -> Int
countValid validator passwords = 
  sum [1 | line <- passwords, validator $ words line]

main = do
  input <- readFile "day4-input.txt"
  let passwords = lines input
  putStrLn $ "part1: " ++ show (countValid policy1 passwords)
  putStrLn $ "part2: " ++ show (countValid policy2 passwords)
