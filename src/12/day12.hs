import Data.List.Split (splitOn)
import Data.Map ((!))
import Text.Regex
import qualified Data.Map as Map
import qualified Data.Set as Set

groupSize :: Map.Map Int [Int] -> Int -> Int
groupSize graph fromId = traverse Set.empty [fromId]
  where traverse visited toVisit =
          case toVisit of
            (id:xs) -> if id `Set.notMember` visited
                       then 1 + traverse (Set.insert id visited)
                                         (graph ! id ++ xs)
                       else traverse visited xs
            _ -> 0

parseLine :: String -> (Int, [Int])
parseLine line = (id, peers)
  where lineRegex = mkRegex "([0-9]+) <-> (.*)"
        Just [idStr, peersStr] = matchRegex lineRegex line
        id = read idStr::Int
        peers = map (\x -> read x::Int) (splitOn ", " peersStr)

main = do
  graph <- Map.fromList . map parseLine . lines <$>
           readFile "input.txt"
  print $ length graph
  print $ groupSize graph 0
