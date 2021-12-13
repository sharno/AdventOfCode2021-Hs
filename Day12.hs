module Day12 where

import qualified Data.Map as M
import Data.Maybe
import Data.List

type Graph = M.Map String [String]
type Path = [String]

-- PART 1
graph :: [(String, String)] -> Graph
graph xs = foldl f M.empty $ xs ++ [(y,x) | (x,y) <- xs]
  where
    f :: Graph -> (String, String) -> Graph
    f acc (from, to) = M.insertWith (++) from [to] acc

bfs :: Graph -> (Path -> Bool) -> [Path] -> [Path]
bfs _ _ paths | all ((== "end") . head) paths = paths
bfs g validate paths = bfs g validate $ finishedPaths ++ onlyValidPaths
  where
  finishedPaths = filter ((== "end") . head) paths
  remPaths = filter ((/= "end") . head) paths
  expand path = map (:path) $ fromJust $ M.lookup (head path) g
  expandedPaths = concatMap expand remPaths
  onlyValidPaths = filter validate expandedPaths


validate1 (node:p) = not $ isSmallCave && isVisited
  where
    isSmallCave = flip elem ['a'..'z'] $ head node
    isVisited = elem node p

day12p1 = length $ bfs (graph input) validate1 [["start"]]

-- PART 2
validate2 path = elem (head $ head path) ['A'..'Z'] || (not 
  $ head path == "start"
  || M.filter (> 2) smallCounts /= M.empty
  || length (M.filter (> 1) smallCounts) > 1
  )
  where
    smallCounts = foldl (\acc node -> if isSmallCave node then M.insertWith (+) node (1::Int) acc else acc) M.empty path
    isSmallCave x = elem (head x) ['a'..'z']


day12p2 = length $ bfs (graph input) validate2 [["start"]]

test = [
  ("start","A"),
  ("start","b"),
  ("A","c"),
  ("A","b"),
  ("b","d"),
  ("A","end"),
  ("b","end")
  ]

test2 = [
  ("dc","end"),
  ("HN","start"),
  ("start","kj"),
  ("dc","start"),
  ("dc","HN"),
  ("LN","dc"),
  ("HN","end"),
  ("kj","sa"),
  ("kj","HN"),
  ("kj","dc")
  ]

test3 = [
  ("fs","end"),
  ("he","DX"),
  ("fs","he"),
  ("start","DX"),
  ("pj","DX"),
  ("end","zg"),
  ("zg","sl"),
  ("zg","pj"),
  ("pj","he"),
  ("RW","he"),
  ("fs","DX"),
  ("pj","RW"),
  ("zg","RW"),
  ("start","pj"),
  ("he","WI"),
  ("zg","he"),
  ("pj","fs"),
  ("start","RW")
  ]

input =     [
  ("GC", "zi"),
  ("end", "zv"),
  ("lk", "ca"),
  ("lk", "zi"),
  ("GC", "ky"),
  ("zi", "ca"),
  ("end", "FU"),
  ("iv", "FU"),
  ("lk", "iv"),
  ("lk", "FU"),
  ("GC", "end"),
  ("ca", "zv"),
  ("lk", "GC"),
  ("GC", "zv"),
  ("start", "iv"),
  ("zv", "QQ"),
  ("ca", "GC"),
  ("ca", "FU"),
  ("iv", "ca"),
  ("start", "lk"),
  ("zv", "FU"),
  ("start", "zi")]
  