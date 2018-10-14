import Data.List.Split (splitOn)

readInt :: String -> Int
readInt x = read x::Int

reverseRange :: Int -> Int -> Int -> Int -> Int
reverseRange n i start length = if (i >= start && i < start + length) ||
                                   (i < start && i + n < (start + length))
  then (start + length - (i - start) - 1) `mod` n
  else i

valueAtIndex :: Int -> [Int] -> Int -> Int
valueAtIndex n lengths endIndex = startIndex
  where (startIndex, 0, 0) = foldr undoReverse endState lengths
        endSkip = length lengths `mod` n
        endPos = (sum lengths + sum (take (length lengths) [0..])) `mod` n
        endState = (endIndex, endSkip, endPos)
        undoReverse length (i, skip, pos) =
          let prevSkip = (skip - 1) `mod` n
              prevPos = (pos - length - prevSkip) `mod` n
          in (reverseRange n i prevPos length, prevSkip, prevPos)

main = do
  lengths <- map readInt . splitOn "," <$> readFile "day10-input.txt"
  let n = 256
  print $ valueAtIndex n lengths 0 * valueAtIndex n lengths 1
  
