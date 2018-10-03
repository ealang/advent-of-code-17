applyCancel :: String -> String
applyCancel ('!':_:xs) = applyCancel xs
applyCancel (c:xs) = c:applyCancel xs
applyCancel [] = []

removeGarbage :: String -> String
removeGarbage ('<':xs) = removeGarbage $ tail $ dropWhile (/='>') xs
removeGarbage (c:xs) = c:removeGarbage xs
removeGarbage [] = []

countGarbage :: String -> Int
countGarbage ('<':xs) = let (garbage, remain) = break (=='>') xs
                        in length garbage + countGarbage (tail remain)
countGarbage (_:xs) = countGarbage xs
countGarbage [] = 0

countGroups :: Int -> String -> Int
countGroups depth ('{':xs) = depth + countGroups (depth + 1) xs
countGroups depth ('}':xs) = countGroups (depth - 1) xs
countGroups depth (_:xs) = countGroups depth xs
countGroups depth [] = 0

part1 :: String -> Int
part1 = countGroups 1 . removeGarbage . applyCancel

part2 :: String -> Int
part2 = countGarbage . applyCancel

main = do
  input <- readFile "day9-input.txt"
  print $ part1 input
  print $ part2 input
