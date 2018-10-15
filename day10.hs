import Data.Bits (xor)
import Data.Char (ord)
import Data.List (intercalate)
import Data.List.Split (splitOn, chunksOf)
import Text.Printf (printf)

reverseRange :: Int -> Int -> Int -> Int
reverseRange i start length = if (i >= start && i < start + length) ||
                                 (i < start && i + 256 < (start + length))
  then (start + length - (i - start) - 1) `mod` 256
  else i

data CSumState = CSumState {
  index :: Int,
  skip :: Int,
  pos :: Int
} deriving (Show)

checksumPass :: CSumState -> [Int] -> CSumState
checksumPass = foldr undoReverse
  where undoReverse length CSumState { index = i, skip = skip, pos = pos} =
          let prevSkip = (skip - 1) `mod` 256
              prevPos = (pos - length - prevSkip) `mod` 256
          in CSumState {
               index = reverseRange i prevPos length,
               skip = prevSkip,
               pos = prevPos
             }

checkSumPasses :: [Int] -> CSumState -> [CSumState]
checkSumPasses lengths = iterate (`checksumPass` lengths)

checkSumList :: Int -> [Int] -> [Int]
checkSumList passes lengths = map checkSumAtIndex [0..255]
  where checkSumAtIndex i = let endState = CSumState { index = i, skip = endSkip, pos = endPos }
                            in index $ checkSumPasses lengths endState !! passes
        endSkip = length lengths * passes
        endPos = (sum [0..(length lengths * passes - 1)] +
                  sum lengths * passes) `mod` 256

checksumStr :: Int -> String -> String
checksumStr passes str = 
  let standard = [17, 31, 73, 47, 23]
      lengths =  (++standard) . map ord $ str
  in intercalate "" $ map (printf "%02x" . foldl xor 0)
                    $ chunksOf 16
                    $ checkSumList passes lengths

part1 :: IO ()
part1 = do
  lengths <- map (\x -> read x :: Int) . splitOn "," <$> readFile "day10-input.txt"
  let passes = 1
  print $ product $ take 2 $ checkSumList passes lengths

part2 :: IO ()
part2 = do
  str <- takeWhile (/='\n') <$> readFile "day10-input.txt"
  let passes = 64
  print $ checksumStr passes str

main = do
  part1
  part2
