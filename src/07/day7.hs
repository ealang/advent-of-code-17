import Data.Map (Map, (!))
import Text.Regex
import Control.Monad (msum)
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.List as L

type Node = String
type Tree = Map Node [Node]
type Weights = Map Node Int

invertTree :: Tree -> Tree
invertTree tree = M.fromList [ (child, [parent]) |
                                 (parent, children) <- M.assocs tree,
                                 child <- children ]

findRoot :: Tree -> Node
findRoot tree = findRootFrom startNode 
  where startNode = head . M.keys $ tree
        invertedTree = invertTree tree
        findRootFrom curNode = case M.lookup curNode invertedTree of
          Just [parent] -> findRootFrom parent
          _ -> curNode

nodeWeight :: Tree -> Weights -> Node -> Int
nodeWeight tree weights node =
  (weights ! node) +
  sum [
    nodeWeight tree weights child |
      child <- M.findWithDefault [] node tree ]

nodeIsBalanced tree weights node =
  let children = M.findWithDefault [] node tree
      childWeights = map (nodeWeight tree weights) children
  in length (L.nub childWeights) <= 1

findUnbalancedNode :: Tree -> Weights -> Node -> Maybe Node
findUnbalancedNode tree weights curNode =
  let children = M.findWithDefault [] curNode tree
      descendants = map (findUnbalancedNode tree weights) children
      thisNode = if nodeIsBalanced tree weights curNode
                 then Nothing else Just curNode
  in msum (descendants ++ [thisNode])

makeNodeBalanced :: Tree -> Weights -> Node -> Int
makeNodeBalanced tree weights node = 
  let childNodes = M.findWithDefault [] node tree
      childWeights = map (nodeWeight tree weights) childNodes
      weightGroups = L.groupBy ((==) `on` fst) $ L.sort $ zip childWeights childNodes
      extractGroupInfo group = (length group, fst . head $ group, snd . head $ group)
      [(1, oddWeight, oddNode), (_, normalWeight, _)] = take 2 $
                                                        L.sort $
                                                        map extractGroupInfo weightGroups
  in normalWeight - (oddWeight - (weights ! oddNode))

parseLine :: String -> (Node, Int, [Node])
parseLine line = (name, read weightStr::Int, words . removeCommas $ childrenStr)
  where Just [name, weightStr, _, childrenStr] = matchRegex lineRegex line
        lineRegex = mkRegex "([a-z]+) \\(([0-9]+)\\)( -> (.*))?"
        removeCommas = map (\c -> if c == ',' then ' ' else c)

main = do
  input <- readFile "day7-input.txt"
  let fragments = map parseLine (lines input)
  let tree = M.fromList [ (node, children) | (node, _, children) <- fragments ]
  let weights = M.fromList [ (node, weight) | (node, weight, _) <- fragments ]

  let root = findRoot tree
  let Just unbalancedNode = findUnbalancedNode tree weights root

  putStrLn $ "part1: " ++ root
  putStrLn $ "part2: " ++ show (makeNodeBalanced tree weights unbalancedNode)
