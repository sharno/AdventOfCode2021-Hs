module Day15 where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.List

-- PART 1
down cavern (i,j) | i >= length cavern - 1 = Nothing
down cavern (i,j) = Just (i+1,j)

right cavern (i,j) | j >= length cavern - 1 = Nothing
right cavern (i,j) = Just (i,j+1)

up cavern (0,j) = Nothing
up cavern (i,j) = Just (i-1,j)

left cavern (i,0) = Nothing
left cavern (i,j) = Just (i,j-1)

expand :: [[Int]] -> Point -> [Point]
expand cavern p = catMaybes [down cavern p, right cavern p, up cavern p, left cavern p]

genGrid :: [[Int]] -> [[(Int, Int)]]
genGrid cavern = [[(x,y) | y <- [0..length (cavern!!0) - 1]] | x <- [0..length cavern-1]]
type Path = ((Int, Int), S.Set (Int, Int))

type Point = (Int, Int)
type PathMap = M.Map Int (S.Set Point)
risk :: Int -> [[Int]] -> PathMap -> S.Set Point -> String
risk count cavern pathMap visited =  --if count == 1 then show expandedPoints else 
    if S.member (width, width) points then (show score)
    else if null expandedPoints then risk (count + 1) cavern remaining newlyVisited
    else risk (count + 1) cavern newPaths newlyVisited
    where
    width = length cavern - 1
    (bestPaths, remaining) = fromJust $ M.minViewWithKey pathMap
    (score, points) = bestPaths
    newlyVisited = S.union visited points
    expandedPoints = (S.fromList $ concatMap (expand cavern) points) S.\\ visited
    newPaths = foldl (\acc (x,y) -> 
        M.insertWith (S.union) (score + cavern!!x!!y) (S.singleton (x,y)) acc) 
        remaining 
        expandedPoints


day15p1 = risk 0 input (M.singleton 0 (S.singleton (0,0))) S.empty

-- PART 2
increase :: Int -> [[Int]] -> [[Int]]
increase n grid = map (map (\x -> if rem (x + n) 9 == 0 then 9 else rem (x + n) 9)) grid

increaseRows :: [[Int]] -> [[Int]]
increaseRows grid = map (\row -> [inc x n | n <- [0..4], x <- row]) grid
    where
        inc x n = if rem (x + n) 9 == 0 then 9 else rem (x + n) 9

increaseCols :: [[Int]] -> [[Int]]
increaseCols grid = transpose $ increaseRows $ transpose grid

day15p2 = risk 0 d (M.singleton 0 (S.singleton (0,0))) S.empty
    where
    d = increaseCols $ increaseRows input

-- INPUT
test = [
    [1,1,6,3,7,5,1,7,4,2],
    [1,3,8,1,3,7,3,6,7,2],
    [2,1,3,6,5,1,1,3,2,8],
    [3,6,9,4,9,3,1,5,6,9],
    [7,4,6,3,4,1,7,1,1,1],
    [1,3,1,9,1,2,8,1,3,7],
    [1,3,5,9,9,1,2,4,2,1],
    [3,1,2,5,4,2,1,6,3,9],
    [1,2,9,3,1,3,8,5,2,1],
    [2,3,1,1,9,4,4,5,8,1]
    ]
input = [
    [4,6,4,4,1,9,1,1,7,1,3,3,7,7,3,2,1,4,3,7,1,2,1,8,6,1,2,4,2,3,3,5,7,3,9,6,9,3,2,2,3,1,9,9,9,7,1,4,9,3,4,3,2,2,1,5,4,2,3,2,3,2,1,1,3,2,1,2,1,2,1,6,9,1,4,2,2,3,1,2,3,5,6,1,9,9,6,9,7,3,9,4,8,3,7,9,2,4,7,1],
    [2,2,2,1,2,9,3,3,9,2,9,1,1,3,7,1,4,1,1,5,9,3,6,3,9,9,6,2,9,5,1,1,4,1,7,5,2,8,1,3,3,1,3,9,2,1,3,5,7,1,2,9,6,1,3,9,3,8,9,3,1,5,3,3,6,2,1,9,1,8,5,8,3,6,8,3,1,3,4,1,5,2,9,6,5,4,9,2,6,4,6,6,4,2,5,3,8,1,1,6],
    [2,7,9,5,7,9,1,2,9,7,5,5,5,3,1,3,4,1,1,3,8,2,7,6,1,8,2,5,5,6,7,3,3,1,4,7,1,8,1,9,2,1,2,3,1,5,3,4,1,8,2,5,9,1,1,2,3,2,9,4,3,1,5,1,9,3,6,4,1,5,2,1,8,1,1,9,3,2,1,5,1,3,1,2,1,9,1,6,9,1,2,2,6,1,6,9,7,5,9,7],
    [1,2,1,4,5,3,7,6,1,3,9,9,2,1,2,5,7,1,2,3,8,2,2,3,3,9,1,6,9,3,7,2,6,4,8,8,5,9,3,8,5,4,9,1,3,1,3,3,2,8,7,6,3,9,1,6,4,6,1,1,2,2,2,1,2,1,3,1,4,4,2,4,2,1,2,6,2,8,7,7,1,4,5,1,6,4,3,6,1,1,9,1,9,1,9,1,7,3,6,2],
    [1,4,1,2,8,1,1,5,1,5,3,2,1,2,2,1,1,1,1,4,3,9,1,8,2,4,6,4,2,2,1,3,3,1,3,2,9,9,7,4,2,8,3,6,2,3,9,7,4,2,4,1,1,1,3,5,4,8,4,2,7,1,2,8,4,2,2,1,1,2,1,9,2,1,1,8,9,8,3,3,8,4,4,6,3,2,3,2,1,2,1,8,4,3,1,2,1,1,4,1],
    [3,2,4,5,2,5,5,6,7,5,4,1,2,3,5,1,9,9,1,1,9,1,2,5,9,5,9,1,5,6,1,1,1,1,9,9,1,2,2,1,2,6,5,2,1,4,2,2,1,2,9,1,1,7,5,1,3,7,1,6,5,2,9,9,3,7,3,3,7,9,8,2,1,3,2,3,1,4,2,9,5,1,7,1,9,3,9,3,6,2,4,4,9,6,3,5,5,1,7,3],
    [7,8,2,1,6,3,6,4,3,9,1,3,7,1,1,4,4,5,4,8,1,5,2,3,4,1,8,4,4,3,1,1,3,1,1,5,1,8,8,5,5,6,5,8,1,1,1,6,2,1,2,2,1,4,1,1,1,1,2,9,1,7,3,9,1,4,1,2,3,6,1,1,1,3,1,9,9,1,3,3,3,3,8,1,1,1,1,1,6,5,6,9,1,3,2,4,1,5,2,3],
    [1,4,1,8,1,9,1,4,2,1,2,4,2,3,7,2,2,4,1,5,2,3,3,4,3,8,2,8,1,4,1,1,2,2,6,2,4,3,2,9,1,1,6,1,1,1,7,5,4,3,9,7,1,1,3,5,4,8,9,8,2,2,9,3,1,9,1,4,1,1,1,1,1,1,4,4,2,6,1,2,2,9,9,4,4,1,2,4,8,3,3,1,1,1,8,4,7,3,7,6],
    [1,4,1,5,2,2,3,8,1,1,9,2,8,5,8,8,1,1,4,1,6,8,2,1,3,3,1,9,5,9,1,1,4,8,9,2,8,5,1,7,4,2,7,1,6,9,3,8,1,1,8,4,9,9,1,6,9,1,1,1,2,1,1,8,2,2,9,2,1,8,2,6,3,6,3,8,9,5,1,8,5,4,7,4,1,1,2,2,7,2,2,1,5,9,9,3,1,2,1,3],
    [1,1,8,5,1,1,2,2,1,1,2,1,6,2,9,6,8,4,6,1,4,1,2,3,1,6,2,1,5,9,2,1,5,8,5,1,1,2,4,3,1,1,1,1,1,1,3,4,6,1,1,7,6,4,8,9,7,1,9,1,1,3,3,1,8,1,1,2,7,9,6,8,9,2,3,4,2,9,1,8,3,7,1,1,6,1,5,1,2,6,4,7,6,1,2,9,9,5,2,1],
    [2,1,5,7,1,7,5,1,3,2,8,4,2,9,1,8,5,9,2,3,7,6,3,8,6,2,3,5,4,5,2,6,8,6,1,1,1,3,2,4,6,2,7,2,1,4,1,2,2,7,3,3,2,9,8,5,4,4,1,1,3,1,1,1,5,3,4,7,1,2,2,1,2,5,9,1,3,9,3,2,6,2,3,1,3,8,3,2,1,6,5,4,3,7,9,1,2,1,1,3],
    [1,1,1,6,1,1,2,4,9,3,3,1,9,2,9,9,4,3,2,1,4,5,6,4,9,1,4,9,9,9,2,6,9,7,7,1,1,7,2,5,4,2,1,1,5,4,4,2,4,2,2,4,3,1,2,2,1,3,3,9,6,9,7,7,1,4,2,9,8,5,5,1,2,2,3,2,4,2,7,3,2,6,3,3,8,2,9,9,6,4,2,1,4,9,8,3,6,1,9,6],
    [6,5,4,9,2,2,9,4,9,9,2,1,2,3,9,5,1,1,3,2,8,3,2,9,1,4,9,6,1,5,1,2,3,1,6,1,1,4,3,1,2,5,1,8,4,3,3,4,4,1,2,9,1,4,2,2,9,4,3,1,9,9,4,7,2,5,2,8,1,1,2,5,1,2,1,9,7,5,5,3,9,1,6,4,2,5,5,3,1,7,3,1,1,3,8,2,1,2,9,6],
    [4,1,1,3,3,9,3,6,2,1,3,1,6,2,2,2,2,8,3,8,5,2,9,1,4,5,1,9,9,6,8,2,3,8,2,5,6,1,1,2,7,1,1,1,3,8,4,5,5,5,4,8,5,3,8,1,9,1,5,9,2,3,2,4,2,3,6,2,7,7,7,4,3,1,4,1,1,9,8,9,7,9,9,5,9,4,7,1,8,6,2,7,1,2,1,6,8,2,2,1],
    [1,7,4,1,1,2,8,1,3,4,3,5,4,4,1,4,2,4,1,4,1,9,1,7,6,1,1,7,1,1,9,2,1,2,6,4,1,5,4,1,1,1,7,8,8,1,9,5,5,1,2,6,1,6,1,9,4,6,2,1,3,1,1,8,1,9,3,3,1,2,3,2,4,8,5,7,6,1,9,8,7,4,3,4,8,2,8,3,4,2,3,3,2,5,5,4,8,5,9,8],
    [3,7,5,1,1,4,1,1,2,3,7,1,2,1,6,3,9,2,5,1,1,3,2,8,2,1,7,2,7,6,4,9,3,2,3,5,3,5,3,4,1,1,6,5,4,6,2,3,1,2,8,1,2,9,3,1,1,8,5,9,1,4,2,2,5,2,2,1,1,3,1,4,9,1,5,2,2,6,1,4,9,8,8,7,4,5,1,5,9,8,9,3,1,3,5,5,4,3,1,4],
    [4,5,1,4,5,5,1,9,7,1,3,4,1,7,6,1,2,5,1,6,2,9,4,6,5,3,1,2,5,1,1,3,2,9,2,9,1,9,7,8,5,1,2,1,4,8,4,6,1,9,9,1,6,1,2,9,5,7,4,1,1,3,2,7,6,2,1,2,2,4,1,9,2,1,1,1,1,2,6,2,9,6,3,7,9,2,1,5,8,7,1,2,3,3,8,1,1,4,8,1],
    [3,2,9,3,2,1,7,1,4,4,1,3,6,1,1,6,2,9,8,9,9,1,3,4,3,1,1,1,5,2,4,6,1,6,8,6,3,8,9,3,2,3,4,4,6,2,1,1,1,7,2,3,5,3,1,2,1,9,1,4,1,1,7,2,2,7,3,2,6,1,1,9,2,4,2,1,9,1,1,1,8,6,6,1,9,1,5,9,6,1,5,2,9,9,1,5,1,1,3,5],
    [7,7,7,4,2,2,1,1,5,1,2,8,5,2,1,9,9,9,6,1,1,7,1,7,4,3,7,3,4,4,6,1,4,6,1,4,1,8,3,3,8,1,1,6,6,6,6,2,7,8,3,1,9,1,2,9,2,1,2,1,1,6,8,6,3,1,1,3,3,1,6,2,2,3,2,6,6,4,1,1,2,3,2,9,1,5,9,6,3,5,3,8,5,6,6,1,2,9,5,9],
    [1,3,5,1,1,2,2,2,1,4,7,5,1,8,8,9,1,9,5,3,5,1,7,6,4,1,4,3,3,3,2,6,1,7,1,2,7,2,3,1,5,2,9,1,4,4,6,6,7,7,2,1,2,2,1,1,2,9,8,1,1,1,4,2,1,5,1,6,8,6,1,1,2,4,4,7,1,6,1,4,7,2,2,3,2,9,1,2,1,1,4,9,1,1,4,9,8,4,3,2],
    [7,2,2,5,1,2,1,6,1,7,1,2,5,2,5,7,5,9,8,8,7,1,9,6,3,2,1,1,8,1,5,8,4,4,7,3,7,2,2,1,5,7,8,1,2,4,2,4,8,7,2,7,1,3,2,4,9,8,8,5,1,2,5,1,3,9,1,5,1,3,3,2,3,1,3,3,1,6,5,9,2,2,8,1,8,7,8,4,8,5,1,3,4,4,1,6,1,9,1,6],
    [1,1,1,2,1,8,7,3,4,5,1,4,1,2,9,7,1,7,9,1,9,8,1,4,1,5,1,2,8,1,3,7,2,5,4,3,3,1,1,9,2,1,3,9,2,5,6,9,6,9,1,1,5,7,7,2,2,7,2,4,1,2,2,2,8,1,6,4,7,1,1,5,1,8,5,5,1,9,4,1,1,2,2,2,1,6,7,2,6,1,2,4,6,8,1,2,2,1,1,9],
    [1,1,2,1,9,9,1,3,5,5,9,9,2,1,3,2,5,5,2,1,1,1,2,1,3,1,3,2,1,9,1,4,4,2,5,2,9,4,3,3,3,7,9,2,2,2,1,4,1,7,3,7,7,2,1,1,1,8,5,1,5,8,1,4,4,1,1,1,7,1,1,2,8,3,7,1,1,5,8,3,9,1,1,8,5,2,1,1,7,7,2,6,8,1,1,2,1,1,5,1],
    [3,6,1,1,3,1,7,1,1,9,8,4,9,2,8,5,6,1,1,9,3,1,2,1,6,3,8,1,8,1,4,8,4,1,1,1,7,1,1,1,5,5,3,1,1,8,1,2,3,1,5,1,1,2,3,4,2,9,1,2,1,9,3,3,7,3,3,7,8,1,4,3,1,9,1,8,2,4,6,4,3,8,3,1,9,1,9,2,5,4,3,3,7,8,2,4,1,2,4,4],
    [2,2,1,6,9,7,3,1,5,4,3,3,5,1,9,7,1,3,5,2,1,1,5,3,5,8,2,1,4,8,2,8,1,6,1,3,6,1,2,1,2,1,6,7,7,7,1,3,8,7,2,2,3,5,7,7,2,6,1,2,2,6,3,1,2,5,3,4,3,1,1,9,1,2,1,7,2,6,2,2,1,9,6,3,7,1,1,2,9,2,7,4,2,7,5,6,4,1,5,1],
    [5,1,9,1,9,9,8,4,1,1,8,2,3,1,1,8,8,9,3,3,8,5,2,1,7,3,9,2,3,1,2,1,4,9,1,2,4,8,8,1,2,1,6,9,8,1,9,8,8,1,4,2,4,5,5,3,9,1,1,2,1,9,1,1,7,4,1,5,9,9,9,2,5,2,9,1,7,1,2,1,3,1,9,3,9,1,1,2,9,8,4,6,2,2,5,9,4,1,6,2],
    [9,5,1,2,5,3,2,1,6,9,8,2,3,4,6,2,1,1,7,9,4,5,3,9,3,4,3,2,5,4,1,7,4,5,8,1,3,5,2,5,8,1,1,4,4,1,2,7,2,9,8,4,1,2,1,1,1,2,5,5,2,9,3,2,9,1,7,8,1,7,9,2,3,3,8,3,1,8,1,4,7,5,4,1,1,4,4,1,1,5,1,3,4,9,2,2,6,8,2,1],
    [3,4,1,1,8,7,4,8,2,1,9,1,1,5,6,9,2,7,1,3,2,4,1,2,3,2,1,1,3,2,6,1,6,3,8,4,1,9,9,1,1,2,8,1,2,2,3,4,1,4,8,2,7,2,1,1,1,1,1,1,5,3,9,9,6,5,1,2,9,9,3,1,4,6,9,9,2,6,6,6,5,8,8,4,9,9,1,1,7,1,1,2,3,1,3,2,3,9,1,2],
    [7,6,9,4,6,8,5,8,6,1,1,3,2,3,4,8,9,5,1,4,7,1,8,6,1,8,1,7,7,3,1,7,8,7,6,8,5,6,1,5,3,5,2,7,5,3,1,7,2,1,3,9,7,4,4,3,9,2,7,2,1,5,1,2,2,5,5,3,6,1,2,5,1,4,9,2,2,2,1,9,1,1,1,2,7,1,2,2,6,1,3,8,4,5,3,3,4,2,9,6],
    [1,1,1,3,6,9,7,4,1,3,3,2,9,1,7,1,2,1,1,4,3,9,7,3,9,5,1,1,9,3,1,4,1,1,9,2,4,9,6,8,8,8,8,8,3,9,3,1,2,2,7,8,3,1,9,7,5,9,4,1,9,2,3,9,8,1,4,9,7,2,8,1,1,1,2,1,8,5,4,5,3,3,3,9,9,2,6,9,7,4,2,3,1,8,2,6,4,8,6,2],
    [3,2,6,1,6,6,7,1,4,1,1,9,1,7,2,1,2,3,3,3,9,2,3,1,1,9,4,1,1,1,3,6,3,3,1,9,1,7,9,5,8,2,2,1,2,2,2,1,2,1,1,9,2,2,1,1,7,4,1,4,2,6,3,5,6,9,1,7,5,5,5,5,1,2,4,5,1,9,2,3,1,9,9,4,9,1,7,6,8,1,4,8,1,4,3,1,6,9,9,1],
    [2,1,2,8,1,1,1,3,2,5,1,1,4,2,1,2,3,3,6,3,3,7,6,3,6,6,1,2,5,2,9,2,1,7,1,1,2,2,9,3,4,2,6,9,7,6,2,5,3,8,1,4,9,8,9,8,9,3,3,3,1,3,8,6,5,2,2,7,1,3,4,1,8,2,8,1,5,7,1,9,1,5,9,1,1,9,2,2,4,5,7,7,1,2,1,3,3,1,3,1],
    [8,3,8,8,9,1,1,3,7,6,6,1,8,3,4,4,9,9,4,9,9,2,5,8,3,1,8,4,7,5,2,6,8,5,5,3,9,2,3,8,5,4,3,9,5,9,9,5,3,2,4,6,3,6,4,8,2,1,6,5,9,1,2,2,4,2,5,8,8,9,2,9,9,7,2,9,3,5,2,7,5,1,1,1,1,9,2,1,7,2,1,5,1,1,2,7,2,1,9,9],
    [2,1,2,2,1,5,5,9,2,1,9,3,2,5,4,2,1,1,4,6,1,1,1,5,1,4,2,1,7,5,2,1,2,7,8,2,5,7,2,7,1,4,1,1,9,8,1,9,7,4,1,3,1,1,4,2,5,5,4,1,9,1,4,2,7,1,7,2,6,5,3,3,3,3,9,2,6,8,4,8,2,1,4,7,7,2,2,6,6,1,2,8,9,4,8,6,2,5,5,9],
    [1,9,5,1,1,2,1,6,5,5,9,2,1,8,8,1,3,7,5,8,1,9,2,5,3,1,1,9,5,5,4,4,1,3,5,2,5,1,8,1,7,5,7,2,1,1,2,7,1,9,1,1,7,9,9,9,1,5,2,1,1,5,5,1,2,9,9,9,2,1,6,1,2,3,5,2,9,5,2,1,1,3,9,5,3,9,2,4,9,5,5,8,5,1,7,3,7,8,5,5],
    [8,1,9,5,2,1,4,2,8,5,6,1,1,1,8,8,3,9,6,6,1,1,3,3,5,7,9,3,7,1,1,1,1,7,1,5,1,3,1,2,3,1,6,1,8,8,1,4,9,8,7,3,4,2,2,6,1,3,9,7,3,8,2,5,9,8,9,1,8,5,9,2,4,5,1,4,9,9,1,3,5,9,3,4,6,3,3,9,7,8,3,8,4,3,8,4,3,3,3,4],
    [5,4,5,6,8,3,3,9,1,9,6,1,9,2,5,3,9,1,2,1,7,4,7,3,3,9,9,6,9,2,2,1,5,6,2,1,7,8,8,4,3,7,5,9,3,2,1,1,7,3,7,3,5,4,2,9,4,3,2,6,8,9,9,5,8,1,6,1,1,3,2,3,2,1,2,1,7,8,9,7,3,1,3,9,2,8,9,8,4,2,7,3,9,1,1,8,1,1,1,6],
    [4,7,7,7,2,1,7,1,8,1,6,1,1,4,1,4,8,1,2,1,6,6,1,4,4,3,2,3,1,4,6,9,8,1,1,3,8,6,7,7,5,7,8,4,1,5,8,1,1,2,5,3,8,4,2,2,9,4,4,3,2,3,4,1,4,2,3,9,1,9,9,2,2,4,2,2,3,8,1,6,1,4,6,9,1,6,1,9,3,1,9,2,6,3,2,9,2,6,7,3],
    [1,8,7,1,9,8,5,1,2,6,9,5,2,1,3,2,2,7,3,3,1,2,5,3,1,6,1,1,8,2,9,1,1,3,5,1,1,3,3,1,9,6,8,2,5,3,6,3,1,5,1,2,2,5,1,8,6,3,8,8,9,4,4,6,1,9,6,5,7,1,4,3,6,9,8,2,6,6,2,3,1,9,3,9,3,3,1,2,1,1,7,8,1,4,3,1,2,2,2,3],
    [4,1,4,4,1,1,8,6,4,6,8,5,6,2,8,8,7,3,1,7,8,9,1,7,9,9,1,2,6,2,1,3,6,5,5,3,2,7,5,2,7,1,5,1,1,2,2,5,5,9,3,5,5,4,6,1,2,8,4,5,9,2,2,1,1,7,1,3,1,1,1,3,7,7,6,2,9,7,3,8,7,9,1,2,9,4,6,2,1,9,6,8,6,1,2,2,3,3,4,1],
    [5,3,3,1,7,4,3,1,2,6,7,8,7,1,5,4,4,7,1,2,1,2,4,2,5,9,5,2,8,4,5,4,7,6,2,4,5,9,9,1,1,1,2,2,3,4,2,2,2,9,9,8,1,5,4,2,7,2,3,9,9,1,7,5,1,3,2,1,1,2,4,1,3,1,2,3,7,1,5,1,7,1,1,3,6,6,4,1,9,1,1,1,3,1,7,2,9,1,8,2],
    [6,4,5,9,3,2,1,1,2,2,2,8,2,3,1,5,1,3,1,8,2,2,5,2,3,3,1,2,1,1,7,1,1,7,2,1,2,3,2,9,3,8,1,8,2,8,3,1,1,2,6,8,1,5,7,7,3,1,3,1,6,3,3,3,1,2,6,3,9,2,2,8,5,1,8,1,9,5,1,3,8,9,3,7,4,1,2,1,8,6,1,5,7,3,6,1,1,8,8,3],
    [1,1,2,9,5,1,9,1,2,1,9,3,2,1,3,6,1,6,8,2,1,1,1,7,3,3,9,5,1,5,2,1,5,6,1,5,8,3,1,1,1,9,8,5,2,1,1,6,1,7,2,1,8,7,7,2,4,2,1,2,5,6,2,9,6,1,2,1,7,1,3,2,3,5,9,1,2,2,9,3,3,2,8,8,1,6,5,5,5,9,2,2,9,5,1,1,7,4,5,2],
    [4,3,4,1,8,2,2,5,2,1,5,9,5,9,3,9,6,2,2,1,8,9,1,6,4,9,8,1,1,1,6,3,3,1,2,1,9,2,6,1,2,7,4,1,3,2,1,1,8,7,8,1,4,1,1,7,9,6,1,9,3,7,4,1,1,6,9,3,1,1,9,1,8,3,1,1,8,2,2,2,1,7,8,7,7,7,6,4,1,1,1,6,9,9,9,1,1,2,5,4],
    [4,1,9,4,5,3,6,1,2,9,1,7,1,7,1,5,7,2,2,2,8,6,3,2,7,5,1,1,1,1,1,1,2,4,8,2,8,7,4,5,9,7,2,2,7,1,3,2,5,1,1,6,7,1,3,5,6,9,3,7,4,7,5,1,3,2,4,4,2,9,2,9,2,3,1,1,7,5,3,1,5,1,1,3,1,5,1,6,4,1,3,2,4,1,8,3,4,4,4,9],
    [2,6,5,4,1,4,2,9,4,5,6,3,1,1,2,9,9,5,6,1,2,3,3,6,7,4,8,1,1,7,2,9,1,2,1,4,2,7,4,4,1,9,9,1,2,2,1,9,1,6,2,6,9,2,2,2,3,9,3,9,5,1,1,2,5,9,2,2,1,7,1,4,8,8,8,3,5,1,4,1,6,8,9,3,2,6,2,1,1,8,4,2,7,2,1,7,8,6,1,5],
    [4,1,5,7,7,1,3,2,9,4,9,2,1,2,3,2,1,4,4,5,1,9,1,8,2,4,7,6,4,4,1,1,8,9,5,2,6,3,1,1,3,1,1,1,1,4,7,7,7,3,4,2,9,6,5,9,3,6,3,5,6,9,5,1,7,4,3,2,2,5,1,3,1,1,2,1,1,9,1,8,2,4,4,1,6,9,2,4,1,7,8,5,1,2,3,1,8,5,1,1],
    [6,9,1,2,4,1,1,8,2,1,5,3,1,2,9,2,2,9,2,1,8,1,2,6,7,6,1,3,6,6,2,2,2,2,5,1,6,1,3,2,5,7,4,1,9,9,1,3,4,1,7,7,3,9,4,1,4,8,7,9,8,1,1,1,5,1,4,1,3,6,3,1,9,1,2,1,5,6,6,7,3,1,3,1,4,2,3,5,3,1,7,1,6,4,4,3,9,9,1,1],
    [1,4,3,7,3,2,1,1,5,5,3,2,9,2,1,7,2,2,9,1,4,2,6,7,8,1,3,7,5,7,1,1,1,1,3,5,9,2,6,3,4,1,4,1,2,3,2,1,1,2,2,4,3,1,2,8,8,2,3,3,5,8,2,5,1,1,9,9,2,7,7,9,8,1,3,9,8,3,2,9,2,9,1,2,4,1,1,9,6,4,8,1,2,9,2,7,3,1,5,8],
    [1,7,3,2,3,9,1,8,1,8,1,3,8,2,2,9,5,8,9,4,9,1,9,1,4,6,1,9,3,5,8,2,6,6,9,6,1,1,6,3,3,9,8,2,1,3,9,1,9,3,9,5,2,1,3,2,9,1,5,1,4,9,9,9,4,1,5,1,2,2,4,3,5,1,3,2,1,1,3,7,3,4,8,1,3,2,8,8,3,8,1,4,1,9,8,1,9,2,4,9],
    [4,1,1,3,3,4,6,9,2,1,4,2,1,9,1,7,8,6,6,1,1,1,8,5,7,6,5,4,3,9,3,4,1,2,9,5,2,1,2,2,4,1,1,6,8,9,9,3,2,6,1,1,5,5,4,1,2,1,5,7,1,4,8,9,9,2,1,1,9,8,6,8,4,1,9,3,3,3,1,1,4,1,3,1,9,4,6,9,5,5,9,3,6,2,8,9,7,4,1,8],
    [3,9,3,6,1,8,1,5,1,4,2,4,1,7,1,9,5,1,1,9,3,1,2,9,4,3,4,2,1,7,4,1,2,1,3,1,1,3,2,1,2,3,4,1,3,2,8,2,1,6,8,3,4,4,1,1,4,3,2,2,1,6,2,7,1,2,1,2,2,2,9,1,2,2,1,9,1,2,1,1,2,3,4,1,7,2,8,9,7,5,4,9,9,1,2,1,2,1,9,3],
    [5,2,3,1,2,1,1,9,7,9,3,1,1,9,6,1,1,1,6,1,1,6,2,1,2,2,5,1,6,1,3,6,8,2,8,2,8,1,8,1,2,9,1,6,2,7,7,3,1,9,4,9,8,8,3,3,1,5,3,6,7,2,2,1,2,2,7,4,1,1,1,2,2,6,2,5,3,3,8,1,1,6,8,9,7,4,3,6,5,9,5,1,9,9,2,5,3,1,6,2],
    [2,3,1,6,3,1,6,8,2,3,1,3,3,5,2,7,1,8,6,8,5,1,9,4,1,1,8,8,9,1,3,3,1,2,2,1,3,1,4,3,3,4,1,4,1,9,1,1,4,4,1,7,6,2,1,7,2,3,6,7,6,1,1,9,6,4,3,8,6,6,4,1,3,1,1,1,5,1,2,1,3,7,2,4,8,4,9,2,7,9,9,2,7,2,2,8,3,8,7,4],
    [3,1,1,9,1,9,2,1,1,6,8,4,2,7,1,9,2,7,2,5,2,3,1,3,6,6,2,3,9,1,1,3,1,3,9,3,7,5,3,4,1,3,7,5,2,4,2,1,4,4,8,1,1,9,1,7,3,5,1,8,1,4,7,7,3,1,9,3,2,5,6,3,3,1,4,6,1,1,4,9,3,9,1,7,6,4,1,2,1,1,2,6,3,6,3,2,3,4,1,7],
    [2,4,9,2,7,1,4,1,9,2,5,2,1,6,1,2,1,4,1,2,4,8,1,6,9,3,1,9,1,9,3,6,2,6,1,5,1,9,1,1,9,1,9,3,2,1,2,3,4,1,3,1,2,9,6,1,8,1,6,4,2,6,1,9,5,1,1,2,1,2,1,9,1,4,6,6,1,1,4,4,3,2,1,1,6,1,9,5,1,3,2,6,5,9,2,9,9,9,9,5],
    [8,4,7,9,2,8,1,1,3,6,8,1,1,5,1,4,1,6,2,3,3,1,2,3,6,4,2,8,3,5,7,5,9,8,1,1,3,4,9,2,3,4,2,2,5,3,5,6,5,4,1,7,1,5,2,8,7,2,8,4,2,1,1,2,7,2,5,1,4,9,9,3,4,8,6,6,5,3,8,7,4,4,3,3,1,1,4,1,4,4,7,1,1,7,1,6,6,1,1,1],
    [8,1,1,9,6,2,3,4,2,2,4,3,9,2,1,6,2,2,5,2,2,8,1,9,6,5,5,1,9,5,4,2,7,2,6,4,3,4,4,3,1,7,1,9,1,9,5,8,9,1,9,8,6,1,7,8,2,1,8,3,5,1,2,3,2,1,2,3,8,6,8,6,9,1,8,8,4,1,5,9,1,1,4,2,4,1,8,3,9,4,5,1,5,1,3,3,4,2,4,1],
    [2,4,4,1,1,3,1,6,1,3,4,1,1,1,1,6,6,5,3,2,9,7,8,2,2,1,1,6,7,3,6,6,6,2,1,1,4,7,9,7,2,3,1,2,2,8,6,9,9,3,1,5,2,6,2,5,2,7,4,1,1,2,3,4,9,1,9,2,7,9,9,7,6,9,2,1,7,6,5,2,3,1,1,6,9,1,1,2,2,1,6,1,1,2,9,1,9,8,4,8],
    [3,8,3,1,5,4,1,1,9,8,9,9,2,9,2,5,5,5,8,5,3,4,1,3,6,8,5,6,4,4,3,1,4,1,2,7,6,9,9,2,2,3,8,1,8,2,2,5,1,9,6,9,8,1,7,9,9,5,1,3,1,1,3,2,4,2,3,2,7,7,6,1,2,2,1,1,1,1,2,1,9,7,3,1,2,7,1,1,9,9,1,8,3,2,3,2,8,9,3,1],
    [4,8,5,7,2,2,3,1,2,3,9,3,4,3,1,1,2,9,4,1,3,1,9,2,2,7,5,2,9,9,3,1,1,4,9,7,6,9,1,5,5,5,7,9,7,6,8,1,8,1,8,1,2,7,5,7,2,8,8,3,2,3,2,2,2,7,2,4,9,9,2,5,2,3,2,3,1,2,1,3,1,3,2,8,8,1,7,1,7,4,4,3,2,4,7,6,5,4,4,3],
    [6,2,8,8,3,9,1,1,3,4,1,2,4,4,1,6,4,7,3,9,2,4,3,4,3,7,1,6,7,4,5,6,4,9,3,8,1,4,5,3,2,1,8,2,1,2,1,4,3,2,8,1,1,2,7,9,7,1,3,1,7,8,1,1,5,1,1,7,4,8,8,1,3,1,9,9,8,4,2,1,7,5,1,1,4,2,8,8,2,2,5,4,5,2,1,1,4,9,2,4],
    [2,1,9,8,1,9,3,1,1,7,4,2,1,1,6,6,7,9,6,1,2,3,1,9,4,1,9,2,5,3,9,3,1,2,1,4,1,2,1,5,5,1,9,1,1,2,9,7,2,4,5,3,2,3,8,8,9,1,8,1,3,1,1,1,3,6,5,4,4,1,1,1,2,1,1,4,1,1,5,3,4,7,2,1,4,1,1,3,9,1,6,5,7,7,6,1,9,5,1,9],
    [4,8,2,9,2,1,8,1,3,4,7,6,3,3,5,1,1,3,9,2,9,7,3,2,1,3,2,2,1,6,8,9,2,4,1,1,6,1,2,1,4,8,4,5,3,3,2,1,1,2,9,1,2,5,5,7,1,9,3,9,9,3,1,9,2,3,8,3,5,2,1,5,3,1,6,7,8,8,8,2,6,1,3,1,1,8,8,1,1,5,6,3,8,5,9,3,1,4,2,2],
    [1,1,2,8,1,3,1,5,1,8,7,9,5,6,1,2,5,1,1,8,1,1,6,9,8,6,7,1,4,1,1,9,5,1,5,1,3,2,3,3,9,1,4,4,5,9,3,2,1,4,6,1,8,5,2,9,5,2,9,7,8,1,7,3,8,1,8,7,8,9,6,2,1,7,4,3,5,9,1,1,6,6,3,9,2,5,2,1,4,6,4,1,8,1,8,1,6,1,7,8],
    [1,2,3,8,4,9,7,5,1,1,7,7,3,6,9,5,5,1,3,1,1,8,4,3,2,2,2,3,1,9,3,6,5,1,9,1,1,8,2,4,3,3,3,3,8,1,9,4,2,2,1,1,1,1,1,9,1,7,8,9,2,6,3,1,6,5,5,1,9,1,3,2,8,3,6,8,5,5,8,7,1,2,8,4,2,6,1,8,3,7,4,1,6,4,1,9,2,1,4,1],
    [5,1,9,7,8,1,4,2,5,2,6,4,3,1,9,4,5,3,2,1,8,2,1,6,8,1,1,6,5,3,5,5,1,2,1,4,2,1,1,8,9,1,1,4,1,2,9,7,4,1,2,2,5,1,2,4,2,5,2,3,7,5,3,9,8,3,4,3,1,3,4,1,1,2,2,1,1,6,5,7,1,2,4,8,9,9,6,1,9,2,3,5,8,1,1,3,1,9,2,1],
    [5,9,1,1,8,1,2,9,3,9,5,7,9,5,1,8,4,6,5,4,6,1,4,5,1,1,7,3,1,9,8,6,4,6,1,7,1,3,3,5,1,1,1,1,8,5,5,1,1,6,8,7,1,7,3,8,9,1,2,2,9,8,1,1,7,3,4,5,8,9,4,8,8,1,1,1,9,5,2,5,3,5,4,2,1,3,4,1,8,7,1,9,2,9,7,1,2,5,8,8],
    [1,6,7,1,6,5,1,5,2,6,1,8,1,7,6,4,7,7,2,5,9,9,9,2,3,1,4,4,2,1,2,3,1,2,3,1,9,7,3,3,1,4,2,2,3,1,3,6,5,1,5,8,7,3,7,3,7,1,6,3,5,3,7,1,4,2,3,5,2,2,1,4,2,8,2,9,1,8,1,1,9,2,1,1,2,2,6,4,5,2,5,2,2,7,1,7,9,4,9,3],
    [7,4,2,1,1,1,2,8,2,6,8,2,9,2,9,3,1,9,2,1,1,9,8,3,1,2,6,9,3,1,4,9,4,1,2,1,2,9,2,3,2,3,2,1,2,3,5,3,8,8,5,1,8,6,9,1,5,1,2,9,2,1,1,7,1,4,4,7,4,3,9,1,7,6,9,1,1,5,4,5,8,2,4,1,7,4,3,3,9,9,4,4,9,1,5,2,1,9,4,3],
    [9,1,9,1,3,4,6,7,1,6,9,7,2,5,6,1,9,4,5,2,4,5,2,4,4,3,2,4,4,2,8,3,7,1,2,7,9,3,7,3,2,8,2,1,2,6,1,1,6,1,1,1,4,5,1,6,1,8,1,2,9,1,3,3,1,1,2,2,9,3,2,6,2,4,1,4,1,1,3,6,8,9,3,6,9,6,1,4,1,1,1,7,1,1,4,4,5,4,2,1],
    [2,3,1,6,1,6,6,3,1,2,1,9,9,9,9,9,1,2,9,4,5,5,3,9,1,5,2,8,2,7,1,9,3,9,4,9,3,4,2,1,9,6,1,9,8,2,1,6,4,8,1,1,4,9,1,1,1,4,3,1,8,7,9,5,7,2,1,4,3,5,6,1,5,2,9,1,3,6,2,2,1,3,3,1,1,3,1,8,3,2,1,1,8,6,1,2,1,1,5,1],
    [9,9,3,1,2,4,1,1,2,2,8,9,1,9,2,4,2,9,2,3,1,1,4,3,2,3,4,6,6,4,6,1,1,3,8,4,6,7,6,1,5,1,2,1,3,1,9,8,1,4,3,7,3,1,1,5,1,1,5,6,2,1,9,9,3,5,8,1,4,7,2,4,6,7,1,9,1,9,6,7,2,3,8,1,8,1,9,1,3,3,7,7,5,3,2,9,1,3,3,2],
    [1,2,2,2,1,1,4,3,6,1,9,5,2,9,1,4,5,5,5,2,2,1,2,2,2,2,9,6,1,6,9,6,2,2,7,8,4,5,5,8,8,3,6,7,5,1,1,2,1,5,1,8,9,6,6,2,1,9,2,4,4,2,1,5,8,3,1,7,4,6,2,2,9,6,8,5,4,1,4,1,7,2,1,4,1,7,3,7,9,6,3,1,3,3,1,1,4,4,9,3],
    [5,2,1,8,3,1,3,1,1,5,2,2,7,3,1,4,1,3,9,6,5,5,2,4,6,1,8,3,7,6,9,2,1,9,1,2,9,2,4,3,2,2,9,1,1,1,7,4,2,8,5,1,1,5,1,1,8,5,6,1,7,5,2,2,4,1,3,9,7,3,8,2,2,1,2,8,3,4,2,2,2,7,8,1,2,9,1,9,1,6,5,5,8,1,1,6,3,5,2,4],
    [9,2,9,1,1,3,2,4,2,3,1,1,3,2,8,9,2,7,8,3,5,4,3,2,4,5,7,5,1,5,3,1,6,9,2,2,9,9,9,2,1,7,1,1,2,7,1,2,4,1,3,5,4,6,1,8,3,7,1,2,1,2,3,4,5,2,6,2,5,3,1,5,9,3,3,1,6,3,1,6,9,3,4,6,9,2,1,1,2,2,2,9,2,2,1,4,1,9,7,4],
    [8,4,5,2,3,6,3,3,2,5,8,9,7,6,4,1,9,2,5,1,6,9,9,3,2,4,3,1,4,2,8,1,3,9,1,9,9,6,1,3,2,1,8,1,1,1,9,1,5,1,3,6,8,1,2,1,1,2,1,1,9,1,1,1,9,9,5,7,9,5,2,2,2,5,1,1,1,1,3,1,3,1,3,7,2,7,2,3,1,2,1,2,1,2,7,1,1,1,1,8],
    [2,1,9,7,2,2,7,2,1,3,2,7,4,8,1,9,4,8,1,1,2,4,1,9,1,1,4,1,2,2,9,7,1,1,8,6,5,1,9,2,1,1,2,9,4,7,4,6,9,1,1,4,1,4,7,2,1,1,5,2,3,2,3,9,6,4,5,2,1,8,2,1,3,1,5,1,1,9,3,1,3,2,9,4,2,9,2,1,1,2,2,1,1,1,7,9,2,3,7,1],
    [9,2,7,5,3,5,1,1,2,3,1,4,4,9,5,2,3,3,1,9,2,2,2,1,1,6,1,4,3,1,4,2,1,2,3,6,5,5,3,8,5,1,3,5,3,1,1,3,2,5,5,8,1,2,3,1,2,6,9,4,4,5,1,7,8,8,1,1,8,3,1,6,6,1,3,3,2,7,5,2,1,9,9,4,3,3,8,9,5,2,1,1,1,1,1,7,3,1,9,3],
    [1,6,7,6,1,2,3,3,1,1,9,1,3,1,2,9,6,6,2,6,6,5,8,3,1,1,9,1,9,5,1,7,5,9,4,3,6,5,4,1,9,1,2,5,6,4,7,4,8,6,5,2,5,2,8,4,6,9,4,1,9,6,4,9,5,7,1,1,6,1,7,1,2,6,7,9,1,4,7,4,6,9,9,1,2,5,5,3,2,2,5,7,4,1,9,1,3,9,9,6],
    [3,1,9,1,3,5,3,1,2,8,9,5,3,1,3,1,4,2,3,7,1,1,1,3,1,7,1,6,7,1,1,2,8,2,8,6,4,4,7,4,8,3,4,9,5,1,4,4,1,1,1,1,8,2,9,1,1,2,8,3,6,5,1,9,3,4,9,7,2,4,2,2,3,2,9,7,6,1,1,7,1,2,5,3,4,7,2,5,6,1,9,1,9,1,8,1,4,2,2,1],
    [4,1,7,3,7,6,1,1,1,1,1,8,7,3,1,1,9,1,6,3,9,2,9,5,9,6,4,9,1,5,5,9,5,3,2,8,1,4,5,4,6,7,5,1,7,8,2,5,9,4,2,5,3,3,6,9,9,1,9,9,7,9,5,7,2,1,1,9,4,1,3,7,5,1,2,2,9,5,1,3,8,4,1,5,7,1,3,7,9,1,7,8,2,3,3,4,6,8,4,9],
    [6,2,3,1,8,6,6,8,2,1,1,8,7,6,3,2,9,4,3,1,2,1,8,9,2,4,3,3,5,3,1,1,4,2,4,9,9,3,8,3,5,1,9,8,2,1,1,1,4,7,5,6,4,1,1,2,1,4,3,3,1,5,3,4,4,5,1,2,2,1,6,5,1,4,1,2,6,4,4,5,2,2,1,2,4,7,9,1,2,1,7,8,4,8,2,2,9,1,1,2],
    [4,2,1,8,5,1,3,6,1,2,7,1,7,2,2,4,3,2,9,6,2,9,3,6,1,5,9,2,4,8,3,5,1,3,7,3,4,8,1,9,1,3,1,4,1,3,1,1,2,2,3,1,5,1,2,4,2,6,8,3,1,4,3,1,5,9,4,3,3,6,2,2,3,6,1,8,4,1,3,4,7,2,2,4,2,1,2,2,3,1,5,8,1,8,2,1,3,8,2,1],
    [1,6,7,2,5,1,3,5,2,1,1,1,2,6,7,2,5,7,5,7,2,1,2,8,1,9,3,4,5,4,5,3,2,2,1,3,1,8,6,1,6,9,1,4,5,1,1,8,8,2,1,2,5,1,2,2,4,6,4,1,1,6,2,4,9,4,6,3,1,3,8,4,8,5,1,1,2,7,8,3,5,6,3,2,4,3,1,9,1,4,5,9,3,4,7,4,6,1,9,9],
    [7,2,7,7,4,3,5,2,1,5,4,1,3,4,8,5,6,8,1,3,1,1,1,7,7,1,2,1,4,2,9,9,9,7,2,3,1,9,2,7,1,2,5,1,1,2,1,6,2,7,8,3,1,2,4,3,8,1,3,1,1,6,4,6,2,2,3,5,8,1,3,6,5,9,1,7,7,9,4,8,1,3,2,6,9,4,1,1,1,3,2,8,1,1,9,4,8,1,1,8],
    [1,1,7,1,2,5,6,9,9,3,1,9,5,5,8,1,1,2,3,3,2,2,2,4,4,3,9,2,1,1,2,5,3,1,1,1,2,9,2,1,8,2,6,1,1,7,4,5,6,6,3,3,9,5,1,2,2,8,8,9,1,1,1,8,2,1,3,9,7,7,1,3,5,1,1,3,9,1,2,1,2,8,7,3,6,7,7,2,9,2,7,3,9,6,5,1,1,1,5,5],
    [7,5,1,3,4,2,3,6,1,4,8,2,7,2,5,2,9,1,1,4,3,1,2,2,4,4,2,8,1,2,9,4,1,9,4,7,1,9,1,3,8,5,7,3,1,6,3,4,7,9,2,1,8,1,1,2,8,9,6,1,5,2,2,6,2,2,1,5,1,2,9,8,1,2,3,1,3,2,8,4,9,2,5,6,5,2,1,2,8,7,2,1,4,9,8,2,4,4,2,2],
    [4,1,7,2,3,9,2,6,6,7,1,7,2,9,2,3,4,1,4,1,1,2,2,8,1,2,7,1,4,6,8,4,5,5,9,1,3,3,2,1,5,4,2,2,3,4,5,3,9,1,1,1,1,4,3,3,1,1,4,8,8,8,4,4,9,9,3,9,8,6,9,2,6,5,2,6,2,3,2,1,1,1,9,1,2,8,9,9,4,6,7,9,1,9,6,7,3,1,2,9],
    [2,2,6,6,4,2,3,2,7,5,2,7,1,8,3,2,1,9,2,1,9,1,9,2,7,1,3,3,1,6,1,8,5,1,1,1,1,7,1,8,2,4,4,7,1,1,6,1,7,6,1,2,5,9,2,8,2,1,1,6,9,6,3,4,2,1,5,4,9,2,7,2,1,2,1,2,5,2,9,5,1,9,3,3,2,1,7,1,9,2,5,3,7,5,2,1,4,5,1,9],
    [2,9,6,1,2,5,1,3,8,3,6,7,6,3,9,3,3,3,1,9,1,2,7,8,4,8,3,3,1,9,8,3,1,6,8,5,6,1,1,4,6,1,1,4,3,7,9,5,8,5,1,1,1,3,4,7,2,9,5,3,9,8,1,1,3,1,3,3,3,2,4,2,8,1,1,1,4,1,6,1,3,5,4,1,9,1,6,1,1,3,6,8,2,7,3,8,1,5,1,1],
    [2,1,9,1,3,1,3,4,3,2,4,1,4,5,2,3,5,9,4,7,1,9,4,9,3,1,2,6,5,9,9,6,8,4,3,1,3,5,1,3,1,1,9,2,4,3,2,8,1,2,1,2,3,1,1,9,6,8,9,3,2,3,5,5,5,1,3,7,9,7,1,7,5,1,1,1,9,1,1,8,8,8,9,5,4,4,9,8,9,9,9,4,4,7,1,2,9,4,2,2],
    [4,1,1,6,2,2,2,4,3,9,7,9,1,4,9,5,3,3,1,2,2,5,4,1,5,8,5,3,1,3,8,6,3,9,1,4,2,8,2,1,6,9,6,1,8,3,2,7,9,7,8,8,4,7,3,2,2,6,9,1,2,1,3,2,1,6,1,2,9,6,1,1,6,6,3,4,4,4,1,3,5,2,2,4,2,2,8,7,5,1,4,8,3,1,6,4,1,6,3,9],
    [2,4,5,6,1,2,2,9,8,6,4,1,1,1,6,6,4,9,2,9,2,8,2,9,1,6,1,3,2,9,1,4,6,1,7,8,6,6,1,1,1,9,1,2,8,1,4,1,3,5,8,2,8,8,7,2,7,1,8,3,1,1,9,2,1,2,5,3,6,6,1,3,9,1,9,7,1,1,6,4,1,1,7,1,4,1,8,7,3,1,3,7,2,1,1,4,3,3,1,9],
    [3,1,3,7,3,2,1,2,8,6,1,8,1,7,7,3,3,2,3,2,9,7,5,9,9,1,4,1,9,1,1,5,8,9,6,1,3,2,1,4,3,3,3,7,2,4,7,1,5,7,2,1,7,7,1,3,2,6,9,6,2,7,1,6,8,3,3,2,5,1,2,8,9,7,3,1,1,2,1,4,1,7,1,1,4,3,1,1,9,2,9,4,3,2,9,2,3,1,4,1],
    [4,4,7,1,5,9,1,6,4,3,4,2,9,4,1,2,5,3,2,2,1,2,4,1,2,9,2,1,8,9,1,2,6,2,2,2,6,7,3,4,3,2,3,5,1,4,2,5,9,1,1,2,9,8,2,7,2,7,6,1,6,3,1,5,9,3,3,9,1,1,2,9,9,1,4,2,5,2,2,3,8,2,7,4,9,5,9,5,2,1,9,8,9,3,7,7,4,2,1,3],
    [8,3,6,1,4,3,4,4,3,7,2,7,1,4,4,6,1,1,1,5,3,1,1,6,3,7,3,9,8,3,2,7,4,1,7,2,3,9,9,1,1,9,1,3,6,9,2,1,5,8,9,5,1,2,1,4,7,2,2,4,9,7,1,3,8,5,9,4,1,1,7,1,1,3,8,2,8,2,2,9,3,1,1,4,9,3,2,9,5,1,6,3,8,4,8,5,8,8,1,3],
    [7,1,6,2,1,5,1,3,7,6,9,9,5,1,3,8,9,7,7,9,1,4,2,4,2,2,3,2,9,1,2,2,6,3,9,2,1,4,3,1,2,6,3,8,2,4,1,1,6,4,1,1,2,3,7,1,1,5,1,7,9,8,4,7,8,1,6,8,7,1,7,4,5,7,3,6,2,1,6,4,8,4,2,1,5,1,7,3,8,2,1,4,5,3,6,9,1,8,1,4],
    [1,2,2,1,4,1,2,3,5,2,9,7,6,4,4,5,2,1,1,3,1,1,1,4,1,1,9,5,4,7,6,1,9,6,9,9,1,3,2,6,3,8,9,8,8,1,6,1,5,4,1,2,2,1,2,2,8,6,9,6,7,5,4,8,9,5,5,8,5,7,1,3,1,2,7,9,1,2,6,2,6,4,1,1,6,7,1,1,1,2,4,6,6,8,3,3,4,8,9,1],
    [6,9,8,2,4,3,4,6,4,5,8,6,5,3,6,6,3,9,8,4,1,3,5,4,2,1,2,2,2,1,9,7,7,2,3,6,8,6,5,8,4,6,3,7,4,2,6,3,1,1,2,1,9,6,3,3,1,3,8,1,4,9,2,9,8,1,1,1,9,3,1,9,7,2,8,2,5,2,4,2,4,3,2,9,3,2,3,3,2,4,6,4,4,5,6,1,1,6,1,8]
    ]