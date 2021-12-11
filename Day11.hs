module Day11 where

import Data.Maybe
import Data.List

-- PART 1
replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace 0 a (_:xs) = a:xs
replace n a (x:xs) =
  if n < 0
    then (x:xs)
    else x:replace (n-1) a xs

nestedIndices :: [[a]] -> [[(Int, Int)]]
nestedIndices xss = [[(i,j) | j <- [0..length (xss!!0) - 1]] | i <- [0..length xss - 1]]

indices :: [[a]] -> [(Int, Int)]
indices = concat . nestedIndices

up xss (0,j) = Nothing
up xss (i,j) = Just (i-1,j)

down xss (i,j) | i >= length xss - 1 = Nothing
down xss (i,j) = Just (i+1,j)

left xss (i,0) = Nothing
left xss (i,j) = Just (i,j-1)

right xss (i,j) | j >= length xss - 1 = Nothing
right xss (i,j) = Just (i,j+1)

neighborsIndices :: [[Int]] -> (Int, Int) -> [(Int, Int)]
neighborsIndices xss p = catMaybes
  [ up xss p
  , down xss p
  , left xss p
  , right xss p
  , up xss =<< left xss p
  , up xss =<< right xss p
  , down xss =<< left xss p
  , down xss =<< right xss p
  ]

neighbors :: [[Int]] -> (Int, Int) -> [Int]
neighbors xss p = map (\(i,j) -> xss!!i!!j) $ neighborsIndices xss p

flashingNeighbors :: [[Int]] -> (Int, Int) -> Int
flashingNeighbors xss p = length . filter (== 0) $ neighbors xss p

-- Breadth First search for flashing octopuses
bfsFlash :: [(Int, Int)] -> [[Int]] -> [[Int]]
bfsFlash [] curr = curr
bfsFlash ((i,j):toVisit) curr | curr!!i!!j + flashingNeighbors curr (i,j) < 10 = bfsFlash toVisit curr
bfsFlash ((i,j):toVisit) curr = bfsFlash (toVisit ++ neighborsIndices curr (i,j)) $ replace i (replace j 0 $ curr!!i) curr
 
step :: [[Int]] -> (Int, [[Int]])
step xss = (countFlashing, finalEnergy)
  where
  addOne = map (map (\x -> if x + 1 == 10 then 0 else x + 1)) xss
  cascadeFlashing = bfsFlash (indices xss) addOne
  finalEnergy = map (map (\(i,j) -> if cascadeFlashing!!i!!j == 0 then 0 else flashingNeighbors cascadeFlashing (i,j) + cascadeFlashing!!i!!j)) (nestedIndices xss)
  countFlashing = length . filter (== 0) $ concat finalEnergy

repeatSteps 0 count state = count
repeatSteps n count state = repeatSteps (n-1) (count + c) s
  where
  (c, s) = step state

day11p1 = repeatSteps 100 0 input

-- PART 2
allFlash :: Int -> (Int, [[Int]]) -> Int
allFlash n (c, s) = if c == 100 then n else allFlash (n+1) (step s)

day11p2 = allFlash 0 (0, input)

test =     [[5,4,8,3,1,4,3,2,2,3],
    [2,7,4,5,8,5,4,7,1,1],
    [5,2,6,4,5,5,6,1,7,3],
    [6,1,4,1,3,3,6,1,4,6],
    [6,3,5,7,3,8,5,4,7,8],
    [4,1,6,7,5,2,4,6,4,5],
    [2,1,7,6,8,4,1,7,2,1],
    [6,8,8,2,8,8,1,1,3,4],
    [4,8,4,6,8,4,8,5,5,4],
    [5,2,8,3,7,5,1,5,2,6]]

input =     [[5,6,6,5,1,1,4,5,5,4],
    [4,8,8,2,6,6,5,4,2,7],
    [6,1,8,5,5,8,2,1,1,3],
    [7,7,6,2,8,5,2,7,4,4],
    [7,2,5,5,6,2,1,8,4,1],
    [8,8,4,2,7,5,3,1,2,3],
    [8,2,2,5,3,7,2,1,7,6],
    [7,2,1,2,8,6,5,8,2,7],
    [7,7,5,8,7,5,1,1,5,7],
    [1,8,2,8,5,4,4,5,6,3]]