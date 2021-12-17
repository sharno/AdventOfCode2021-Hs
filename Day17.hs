module Day17 where

import Data.Maybe

type State = (Int, Int, Int, Int)
-- PART 1

withinTarget :: State -> Bool
withinTarget (_,_,x,y) = x >= 119 && x <= 176 && y >= -141 && y <= -84

out :: State -> Bool
out (xv,yv,x,y) = (xv == 0 && (x < 119 || x > 176))
    || (yv < 0 && y < -141)

step :: State -> State
step (xv, yv, x, y) =
    ( if xv > 0 then xv - 1 else if xv < 0 then xv + 1 else 0
    , yv-1, x+xv, y+yv)

simulate :: Int -> State -> Maybe State
simulate _ state | withinTarget state = Just state
simulate n _     | n <= 0 = Nothing
simulate _ state | out state = Nothing
simulate n state = simulate (n-1) (step state)

simulate2 :: Int -> [State] -> Maybe [State]
simulate2 _ state@(s:_) | withinTarget s = Just state
simulate2 n _     | n <= 0 = Nothing
simulate2 _ state@(s:_) | out s = Nothing
simulate2 n state@(s:_) = simulate2 (n-1) (step s:state)

-- after some trials 140 seems to be the highest y velocity that can hit the target
testing = filter ((== 140) . snd) $ [(xv,yv) | xv <- [0..177], yv <- [100..200], isJust $ simulate 10000 (xv,yv, 0, 0)]

-- these values are the ones that has y velocity of 140 and can hit target
day17p1 = maximum $ map (\(xv,yv,x,y) -> y) $ concat $ map (fromJust . simulate2 10000 . (\(xv,yv) -> [(xv,yv,0,0)])) $ [(15,140),(16,140),(17,140),(18,140)]

-- PART 2
day17p2 = length $ [(xv,yv) | xv <- [0..177], yv <- [(-141)..140], isJust $ simulate 10000 (xv,yv, 0, 0)]

-- INPUT