module Day18 where

data Tree = Pair Tree Tree | Number Int
    deriving (Show, Eq)
data Ctx = L Tree | R Tree
type Zipper = [Ctx]
type State = (Tree, Zipper)

farthest :: (a -> Maybe a) -> a -> a
farthest f a = case f a of
    Nothing -> a
    Just b -> farthest f b

-- left :: Tree -> Zipper -> Maybe (Tree, Zipper)
-- left (Pair l r) zipper = Just (l, L r:zipper)
-- left _ _ = Nothing

-- right :: Tree -> Zipper -> (Tree, Zipper)
-- right (Pair l r) zipper = (r, R l:zipper)
-- right t zipper = (t, zipper)

up :: State -> Maybe State
up (_, []) = Nothing
up (t, (L r:zipper)) = Just (Pair t r, zipper)
up (t, (R l:zipper)) = Just (Pair l t, zipper)

addLeft :: Int -> Tree -> Tree
addLeft n (Number x) = Number (n + x)
addLeft n (Pair l r) = Pair (addLeft n l) r

addRight :: Int -> Tree -> Tree
addRight n (Number x) = Number (n + x)
addRight n (Pair l r) = Pair l (addRight n r)

data ExplodeState = Done | Remaining
boom :: Zipper -> Tree -> Tree
boom zipper (Pair (Number x) (Number y)) = f zipper (Number 0) Remaining Remaining
    where
    f [] t _ _ = t
    f (L r:z) t lState Remaining = f z (Pair t (addLeft y r)) lState Done
    f (R l:z) t Remaining rState = f z (Pair (addRight x l) t) Done rState
    f (L r:z) t lState rState = f z (Pair t r) lState rState
    f (R l:z) t lState rState = f z (Pair l t) lState rState
boom _ _ = undefined

explode :: Tree -> Maybe Tree
explode = f []
    where
    f _ (Number _) = Nothing
    f zipper t@(Pair _ _) | length zipper > 3 = Just (boom zipper t)
    f zipper (Pair l r) = case f (L r:zipper) l of
        Just res -> Just res
        Nothing -> case f (R l:zipper) r of
            Just res -> Just res
            Nothing -> Nothing

split :: Tree -> Maybe Tree
split t = fst <$> f t []
    where
    f :: Tree -> Zipper -> Maybe State
    f (Number n) zipper | n > 9 = Just $ farthest up (splitted, zipper)
        where splitted = Pair (Number $ div n 2) (Number $ ceiling $ (/) (fromIntegral n) 2)
    f (Number _) _ = Nothing
    f (Pair l r) zipper = case f l (L r:zipper) of
        Just res -> Just res
        Nothing -> case f r (R l:zipper) of
            Just res -> Just res
            Nothing -> Nothing

reduce :: Tree -> Tree
reduce t = let
    exploded = farthest explode t
    in case split exploded of
    Nothing -> exploded
    Just splitted -> reduce splitted

add :: Tree -> Tree -> Tree
add t1 t2 = reduce $ Pair t1 t2

magnitude :: Tree -> Int
magnitude (Number n) = n
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

-- PART 1
day18p1 = magnitude $ foldl1 add input

-- PART 2
day18p2 = maximum [magnitude $ add x y | x <- input, y <- input, x /= y]

-- INPUT

test = [
    (Pair (Pair (Pair (Number 0) (Pair (Number 4) (Number 5))) (Pair (Number 0) (Number 0))) (Pair (Pair (Pair (Number 4) (Number 5)) (Pair (Number 2) (Number 6))) (Pair (Number 9) (Number 5)))),
    (Pair (Number 7) (Pair (Pair (Pair (Number 3) (Number 7)) (Pair (Number 4) (Number 3))) (Pair (Pair (Number 6) (Number 3)) (Pair (Number 8) (Number 8))))),
    (Pair (Pair (Number 2) (Pair (Pair (Number 0) (Number 8)) (Pair (Number 3) (Number 4)))) (Pair (Pair (Pair (Number 6) (Number 7)) (Number 1)) (Pair (Number 7) (Pair (Number 1) (Number 6))))),
    (Pair (Pair (Pair (Pair (Number 2) (Number 4)) (Number 7)) (Pair (Number 6) (Pair (Number 0) (Number 5)))) (Pair (Pair (Pair (Number 6) (Number 8)) (Pair (Number 2) (Number 8))) (Pair (Pair (Number 2) (Number 1)) (Pair (Number 4) (Number 5))))),
    (Pair (Number 7) (Pair (Number 5) (Pair (Pair (Number 3) (Number 8)) (Pair (Number 1) (Number 4))))),
    (Pair (Pair (Number 2) (Pair (Number 2) (Number 2))) (Pair (Number 8) (Pair (Number 8) (Number 1)))),
    (Pair (Number 2) (Number 9)),
    (Pair (Number 1) (Pair (Pair (Pair (Number 9) (Number 3)) (Number 9)) (Pair (Pair (Number 9) (Number 0)) (Pair (Number 0) (Number 7))))),
    (Pair (Pair (Pair (Number 5) (Pair (Number 7) (Number 4))) (Number 7)) (Number 1)),
    (Pair (Pair (Pair (Pair (Number 4) (Number 2)) (Number 2)) (Number 6)) (Pair (Number 8) (Number 7)))
    ]
input = [
    (Pair (Pair (Number 0) (Number 6)) (Pair (Pair (Pair (Number 4) (Number 0)) (Pair (Number 6) (Number 6))) (Pair (Pair (Number 2) (Number 2)) (Number 9)))),
    (Pair (Pair (Number 9) (Pair (Pair (Number 1) (Number 6)) (Pair (Number 6) (Number 0)))) (Pair (Pair (Number 1) (Pair (Number 0) (Number 8))) (Pair (Pair (Number 0) (Number 8)) (Pair (Number 9) (Number 8))))),
    (Pair (Pair (Pair (Number 0) (Pair (Number 2) (Number 1))) (Number 3)) (Pair (Pair (Pair (Number 2) (Number 4)) (Pair (Number 1) (Number 2))) (Pair (Number 7) (Number 5)))),
    (Pair (Pair (Pair (Pair (Number 8) (Number 3)) (Pair (Number 8) (Number 5))) (Pair (Pair (Number 7) (Number 8)) (Pair (Number 5) (Number 5)))) (Pair (Number 9) (Number 2))),
    (Pair (Pair (Number 8) (Pair (Number 1) (Number 9))) (Pair (Pair (Pair (Number 9) (Number 9)) (Pair (Number 9) (Number 2))) (Number 1))),
    (Pair (Pair (Pair (Pair (Number 3) (Number 7)) (Pair (Number 2) (Number 1))) (Pair (Number 0) (Number 9))) (Number 4)),
    (Pair (Pair (Pair (Pair (Number 3) (Number 8)) (Pair (Number 6) (Number 0))) (Pair (Number 0) (Number 7))) (Pair (Pair (Pair (Number 6) (Number 3)) (Pair (Number 2) (Number 0))) (Number 9))),
    (Pair (Pair (Pair (Number 9) (Pair (Number 7) (Number 0))) (Pair (Number 8) (Pair (Number 9) (Number 6)))) (Pair (Pair (Number 5) (Number 6)) (Number 4))),
    (Pair (Pair (Pair (Pair (Number 3) (Number 6)) (Pair (Number 3) (Number 6))) (Pair (Number 0) (Number 2))) (Pair (Pair (Pair (Number 8) (Number 3)) (Number 9)) (Pair (Pair (Number 3) (Number 4)) (Number 8)))),
    (Pair (Pair (Number 7) (Pair (Number 8) (Number 4))) (Number 1)),
    (Pair (Number 6) (Pair (Pair (Number 3) (Pair (Number 5) (Number 6))) (Pair (Number 0) (Number 6)))),
    (Pair (Pair (Pair (Number 7) (Pair (Number 4) (Number 7))) (Pair (Pair (Number 4) (Number 5)) (Pair (Number 4) (Number 3)))) (Pair (Pair (Number 5) (Number 5)) (Pair (Number 0) (Pair (Number 4) (Number 2))))),
    (Pair (Pair (Pair (Number 0) (Pair (Number 2) (Number 9))) (Pair (Pair (Number 2) (Number 4)) (Pair (Number 4) (Number 8)))) (Pair (Pair (Number 8) (Pair (Number 9) (Number 5))) (Pair (Pair (Number 9) (Number 6)) (Number 0)))),
    (Pair (Pair (Pair (Pair (Number 2) (Number 0)) (Pair (Number 9) (Number 7))) (Pair (Pair (Number 3) (Number 2)) (Number 0))) (Pair (Number 7) (Number 7))),
    (Pair (Pair (Number 5) (Pair (Number 2) (Number 1))) (Pair (Pair (Number 3) (Pair (Number 5) (Number 1))) (Pair (Pair (Number 8) (Number 5)) (Pair (Number 1) (Number 8))))),
    (Pair (Pair (Pair (Pair (Number 9) (Number 7)) (Number 6)) (Pair (Pair (Number 7) (Number 8)) (Number 7))) (Pair (Pair (Pair (Number 6) (Number 8)) (Number 9)) (Pair (Pair (Number 9) (Number 5)) (Number 7)))),
    (Pair (Pair (Number 4) (Number 2)) (Pair (Pair (Pair (Number 0) (Number 1)) (Pair (Number 7) (Number 2))) (Pair (Pair (Number 0) (Number 2)) (Pair (Number 5) (Number 5))))),
    (Pair (Pair (Number 1) (Number 8)) (Pair (Pair (Number 5) (Pair (Number 7) (Number 9))) (Pair (Pair (Number 3) (Number 1)) (Pair (Number 7) (Number 1))))),
    (Pair (Pair (Pair (Number 4) (Pair (Number 4) (Number 6))) (Number 6)) (Number 5)),
    (Pair (Pair (Pair (Number 5) (Pair (Number 3) (Number 6))) (Number 6)) (Pair (Pair (Pair (Number 8) (Number 0)) (Pair (Number 8) (Number 6))) (Pair (Pair (Number 3) (Number 3)) (Pair (Number 0) (Number 1))))),
    (Pair (Pair (Number 4) (Pair (Pair (Number 2) (Number 6)) (Pair (Number 0) (Number 9)))) (Pair (Pair (Number 0) (Number 6)) (Pair (Number 4) (Number 2)))),
    (Pair (Pair (Pair (Pair (Number 9) (Number 4)) (Pair (Number 6) (Number 5))) (Number 7)) (Pair (Pair (Pair (Number 1) (Number 5)) (Pair (Number 0) (Number 9))) (Pair (Number 4) (Pair (Number 4) (Number 2))))),
    (Pair (Pair (Number 7) (Pair (Pair (Number 6) (Number 5)) (Number 8))) (Pair (Pair (Pair (Number 5) (Number 6)) (Number 0)) (Pair (Number 6) (Pair (Number 3) (Number 5))))),
    (Pair (Pair (Pair (Number 5) (Pair (Number 6) (Number 4))) (Pair (Number 8) (Pair (Number 0) (Number 4)))) (Pair (Pair (Number 3) (Pair (Number 9) (Number 3))) (Number 4))),
    (Pair (Pair (Pair (Pair (Number 4) (Number 0)) (Number 6)) (Pair (Number 6) (Pair (Number 6) (Number 5)))) (Pair (Pair (Number 9) (Pair (Number 6) (Number 3))) (Pair (Pair (Number 9) (Number 6)) (Number 7)))),
    (Pair (Pair (Pair (Pair (Number 2) (Number 2)) (Number 4)) (Pair (Number 8) (Pair (Number 7) (Number 2)))) (Pair (Number 2) (Number 1))),
    (Pair (Number 5) (Pair (Number 9) (Pair (Pair (Number 5) (Number 9)) (Number 4)))),
    (Pair (Pair (Pair (Number 1) (Pair (Number 7) (Number 7))) (Pair (Pair (Number 2) (Number 2)) (Number 8))) (Pair (Pair (Pair (Number 9) (Number 7)) (Number 5)) (Pair (Number 4) (Number 3)))),
    (Pair (Pair (Pair (Pair (Number 6) (Number 8)) (Number 1)) (Number 1)) (Pair (Number 1) (Pair (Pair (Number 2) (Number 0)) (Number 6)))),
    (Pair (Pair (Pair (Pair (Number 0) (Number 5)) (Number 8)) (Pair (Pair (Number 8) (Number 9)) (Pair (Number 9) (Number 3)))) (Pair (Pair (Pair (Number 5) (Number 5)) (Pair (Number 4) (Number 2))) (Number 2))),
    (Pair (Pair (Pair (Number 9) (Pair (Number 2) (Number 5))) (Pair (Number 6) (Pair (Number 1) (Number 7)))) (Pair (Number 5) (Pair (Number 3) (Pair (Number 2) (Number 2))))),
    (Pair (Pair (Pair (Number 7) (Number 6)) (Number 8)) (Pair (Pair (Pair (Number 1) (Number 9)) (Number 3)) (Pair (Number 5) (Number 2)))),
    (Pair (Number 8) (Pair (Pair (Number 2) (Pair (Number 0) (Number 7))) (Number 8))),
    (Pair (Pair (Pair (Pair (Number 8) (Number 1)) (Pair (Number 0) (Number 0))) (Number 5)) (Number 1)),
    (Pair (Pair (Number 1) (Pair (Pair (Number 4) (Number 8)) (Number 0))) (Pair (Pair (Number 9) (Pair (Number 7) (Number 8))) (Number 5))),
    (Pair (Pair (Pair (Pair (Number 1) (Number 3)) (Number 1)) (Pair (Pair (Number 9) (Number 8)) (Pair (Number 6) (Number 6)))) (Number 5)),
    (Pair (Pair (Pair (Number 3) (Number 2)) (Pair (Pair (Number 0) (Number 5)) (Pair (Number 0) (Number 1)))) (Pair (Pair (Number 9) (Pair (Number 9) (Number 3))) (Pair (Number 4) (Number 9)))),
    (Pair (Pair (Pair (Number 0) (Pair (Number 2) (Number 4))) (Pair (Pair (Number 3) (Number 3)) (Pair (Number 6) (Number 5)))) (Pair (Pair (Number 1) (Pair (Number 2) (Number 1))) (Pair (Pair (Number 3) (Number 4)) (Number 9)))),
    (Pair (Pair (Number 2) (Pair (Number 3) (Pair (Number 7) (Number 6)))) (Pair (Number 5) (Number 5))),
    (Pair (Pair (Pair (Pair (Number 8) (Number 2)) (Number 0)) (Pair (Pair (Number 9) (Number 6)) (Pair (Number 9) (Number 0)))) (Pair (Pair (Pair (Number 6) (Number 2)) (Pair (Number 5) (Number 0))) (Number 9))),
    (Pair (Number 7) (Pair (Number 9) (Number 7))),
    (Pair (Number 3) (Pair (Pair (Pair (Number 5) (Number 5)) (Number 1)) (Pair (Number 8) (Number 5)))),
    (Pair (Pair (Pair (Number 5) (Number 5)) (Pair (Number 5) (Number 6))) (Pair (Number 9) (Number 5))),
    (Pair (Pair (Pair (Number 9) (Number 7)) (Pair (Number 1) (Number 2))) (Pair (Number 8) (Pair (Number 5) (Pair (Number 7) (Number 0))))),
    (Pair (Pair (Pair (Number 1) (Pair (Number 5) (Number 2))) (Pair (Number 7) (Pair (Number 8) (Number 9)))) (Pair (Number 2) (Pair (Pair (Number 4) (Number 5)) (Pair (Number 2) (Number 3))))),
    (Pair (Pair (Pair (Number 4) (Pair (Number 2) (Number 2))) (Pair (Number 5) (Pair (Number 4) (Number 7)))) (Pair (Pair (Pair (Number 0) (Number 3)) (Number 2)) (Pair (Number 5) (Pair (Number 2) (Number 6))))),
    (Pair (Pair (Number 0) (Pair (Pair (Number 6) (Number 5)) (Number 5))) (Pair (Pair (Number 7) (Pair (Number 7) (Number 2))) (Number 3))),
    (Pair (Pair (Pair (Number 4) (Pair (Number 9) (Number 4))) (Pair (Number 1) (Number 9))) (Pair (Number 7) (Pair (Pair (Number 7) (Number 1)) (Pair (Number 6) (Number 1))))),
    (Pair (Number 1) (Pair (Number 0) (Number 2))),
    (Pair (Pair (Pair (Pair (Number 5) (Number 1)) (Pair (Number 2) (Number 1))) (Pair (Pair (Number 7) (Number 8)) (Number 6))) (Pair (Pair (Number 3) (Pair (Number 4) (Number 9))) (Number 2))),
    (Pair (Pair (Number 9) (Pair (Pair (Number 4) (Number 0)) (Pair (Number 8) (Number 8)))) (Pair (Pair (Pair (Number 6) (Number 6)) (Pair (Number 2) (Number 8))) (Pair (Number 1) (Pair (Number 1) (Number 5))))),
    (Pair (Pair (Pair (Number 1) (Number 2)) (Pair (Number 7) (Number 0))) (Pair (Number 7) (Pair (Pair (Number 3) (Number 0)) (Number 5)))),
    (Pair (Pair (Pair (Number 6) (Pair (Number 0) (Number 8))) (Number 3)) (Pair (Pair (Number 3) (Number 7)) (Number 1))),
    (Pair (Pair (Pair (Pair (Number 6) (Number 1)) (Pair (Number 1) (Number 0))) (Number 9)) (Pair (Pair (Number 4) (Number 8)) (Pair (Number 3) (Pair (Number 0) (Number 8))))),
    (Pair (Pair (Number 6) (Pair (Number 3) (Pair (Number 5) (Number 8)))) (Number 9)),
    (Pair (Pair (Pair (Pair (Number 5) (Number 0)) (Pair (Number 7) (Number 7))) (Pair (Pair (Number 3) (Number 1)) (Pair (Number 4) (Number 8)))) (Number 5)),
    (Pair (Pair (Pair (Number 3) (Number 7)) (Pair (Number 9) (Number 0))) (Pair (Pair (Pair (Number 0) (Number 2)) (Number 7)) (Number 0))),
    (Pair (Number 8) (Number 9)),
    (Pair (Pair (Number 8) (Pair (Pair (Number 0) (Number 8)) (Number 4))) (Pair (Number 1) (Pair (Pair (Number 4) (Number 6)) (Number 2)))),
    (Pair (Pair (Pair (Number 5) (Number 5)) (Number 3)) (Pair (Pair (Number 6) (Number 6)) (Pair (Number 0) (Pair (Number 6) (Number 3))))),
    (Pair (Pair (Pair (Number 7) (Pair (Number 3) (Number 7))) (Pair (Pair (Number 6) (Number 1)) (Pair (Number 9) (Number 4)))) (Pair (Pair (Pair (Number 8) (Number 9)) (Number 1)) (Pair (Pair (Number 8) (Number 7)) (Number 6)))),
    (Pair (Pair (Number 6) (Pair (Pair (Number 0) (Number 9)) (Pair (Number 2) (Number 3)))) (Pair (Pair (Number 1) (Pair (Number 5) (Number 3))) (Pair (Number 8) (Number 4)))),
    (Pair (Pair (Pair (Number 3) (Number 5)) (Number 8)) (Pair (Pair (Pair (Number 2) (Number 4)) (Pair (Number 7) (Number 5))) (Number 5))),
    (Pair (Pair (Number 0) (Pair (Pair (Number 7) (Number 0)) (Pair (Number 9) (Number 4)))) (Pair (Pair (Pair (Number 0) (Number 0)) (Pair (Number 6) (Number 7))) (Pair (Number 6) (Number 5)))),
    (Pair (Pair (Pair (Pair (Number 1) (Number 9)) (Pair (Number 6) (Number 4))) (Number 0)) (Pair (Number 6) (Pair (Number 3) (Pair (Number 4) (Number 8))))),
    (Pair (Pair (Pair (Pair (Number 1) (Number 6)) (Pair (Number 0) (Number 4))) (Number 8)) (Pair (Pair (Number 8) (Number 8)) (Number 6))),
    (Pair (Pair (Pair (Pair (Number 7) (Number 4)) (Pair (Number 9) (Number 6))) (Number 7)) (Pair (Pair (Number 1) (Number 6)) (Pair (Number 1) (Number 0)))),
    (Pair (Number 1) (Pair (Pair (Pair (Number 6) (Number 8)) (Number 5)) (Number 5))),
    (Pair (Number 8) (Number 4)),
    (Pair (Number 9) (Pair (Pair (Number 9) (Pair (Number 3) (Number 9))) (Number 0))),
    (Pair (Number 5) (Pair (Pair (Pair (Number 4) (Number 9)) (Number 7)) (Pair (Pair (Number 1) (Number 0)) (Number 0)))),
    (Pair (Pair (Pair (Number 6) (Number 1)) (Pair (Number 0) (Pair (Number 2) (Number 3)))) (Pair (Pair (Pair (Number 7) (Number 8)) (Pair (Number 5) (Number 9))) (Number 9))),
    (Pair (Number 3) (Pair (Pair (Number 3) (Pair (Number 3) (Number 4))) (Pair (Number 6) (Pair (Number 7) (Number 8))))),
    (Pair (Pair (Pair (Number 7) (Pair (Number 7) (Number 1))) (Pair (Number 4) (Pair (Number 2) (Number 0)))) (Pair (Number 6) (Pair (Number 7) (Number 3)))),
    (Pair (Pair (Number 6) (Number 9)) (Pair (Pair (Number 3) (Pair (Number 4) (Number 7))) (Number 3))),
    (Pair (Number 1) (Pair (Pair (Number 9) (Pair (Number 5) (Number 1))) (Pair (Number 7) (Pair (Number 7) (Number 5))))),
    (Pair (Pair (Number 3) (Number 2)) (Pair (Pair (Number 9) (Pair (Number 6) (Number 8))) (Pair (Pair (Number 1) (Number 0)) (Number 2)))),
    (Pair (Pair (Pair (Pair (Number 3) (Number 2)) (Number 8)) (Pair (Number 7) (Number 6))) (Number 9)),
    (Pair (Pair (Number 3) (Pair (Pair (Number 9) (Number 5)) (Number 6))) (Pair (Number 5) (Number 9))),
    (Pair (Pair (Pair (Number 3) (Pair (Number 6) (Number 3))) (Pair (Pair (Number 7) (Number 0)) (Pair (Number 5) (Number 7)))) (Pair (Pair (Number 3) (Number 3)) (Pair (Pair (Number 4) (Number 9)) (Pair (Number 4) (Number 8))))),
    (Pair (Pair (Pair (Number 0) (Pair (Number 4) (Number 3))) (Number 2)) (Pair (Number 3) (Pair (Number 0) (Pair (Number 1) (Number 3))))),
    (Pair (Pair (Pair (Number 7) (Pair (Number 3) (Number 4))) (Pair (Number 7) (Pair (Number 3) (Number 1)))) (Pair (Pair (Number 0) (Pair (Number 4) (Number 7))) (Number 6))),
    (Pair (Pair (Pair (Number 1) (Pair (Number 7) (Number 4))) (Pair (Pair (Number 8) (Number 7)) (Number 3))) (Number 4)),
    (Pair (Pair (Pair (Number 5) (Number 5)) (Pair (Pair (Number 0) (Number 3)) (Number 2))) (Pair (Number 1) (Pair (Pair (Number 9) (Number 4)) (Number 6)))),
    (Pair (Pair (Pair (Pair (Number 6) (Number 0)) (Pair (Number 8) (Number 8))) (Pair (Number 6) (Pair (Number 6) (Number 0)))) (Pair (Number 5) (Number 6))),
    (Pair (Pair (Pair (Number 1) (Pair (Number 5) (Number 4))) (Pair (Pair (Number 5) (Number 9)) (Pair (Number 1) (Number 7)))) (Pair (Pair (Number 5) (Pair (Number 4) (Number 7))) (Pair (Number 4) (Pair (Number 4) (Number 4))))),
    (Pair (Pair (Number 0) (Pair (Pair (Number 2) (Number 6)) (Number 0))) (Pair (Pair (Number 6) (Pair (Number 4) (Number 3))) (Number 5))),
    (Pair (Pair (Pair (Number 1) (Pair (Number 5) (Number 3))) (Pair (Number 9) (Pair (Number 1) (Number 2)))) (Pair (Pair (Pair (Number 4) (Number 8)) (Pair (Number 5) (Number 6))) (Number 0))),
    (Pair (Pair (Number 0) (Number 7)) (Pair (Number 1) (Pair (Number 7) (Number 7)))),
    (Pair (Number 4) (Pair (Pair (Number 7) (Pair (Number 7) (Number 2))) (Pair (Pair (Number 9) (Number 1)) (Number 7)))),
    (Pair (Number 2) (Pair (Pair (Number 1) (Number 6)) (Pair (Number 6) (Number 9)))),
    (Pair (Pair (Pair (Number 4) (Pair (Number 4) (Number 5))) (Number 9)) (Pair (Pair (Pair (Number 1) (Number 7)) (Number 6)) (Pair (Number 3) (Pair (Number 7) (Number 3))))),
    (Pair (Pair (Number 6) (Pair (Pair (Number 1) (Number 1)) (Pair (Number 7) (Number 8)))) (Pair (Pair (Pair (Number 5) (Number 2)) (Pair (Number 8) (Number 1))) (Number 5))),
    (Pair (Pair (Pair (Number 5) (Number 5)) (Pair (Pair (Number 4) (Number 1)) (Pair (Number 1) (Number 2)))) (Pair (Pair (Number 3) (Number 8)) (Pair (Number 3) (Number 4)))),
    (Pair (Pair (Pair (Pair (Number 1) (Number 9)) (Pair (Number 0) (Number 3))) (Pair (Number 4) (Pair (Number 0) (Number 9)))) (Number 4)),
    (Pair (Pair (Pair (Number 4) (Number 9)) (Number 0)) (Pair (Pair (Number 9) (Number 0)) (Pair (Number 8) (Pair (Number 7) (Number 5))))),
    (Pair (Pair (Number 6) (Pair (Number 5) (Number 3))) (Pair (Pair (Pair (Number 6) (Number 6)) (Number 4)) (Pair (Pair (Number 6) (Number 8)) (Number 4)))),
    (Pair (Pair (Pair (Pair (Number 1) (Number 1)) (Number 2)) (Number 1)) (Pair (Number 1) (Pair (Pair (Number 6) (Number 4)) (Number 2)))),
    (Pair (Pair (Pair (Pair (Number 6) (Number 3)) (Pair (Number 1) (Number 5))) (Pair (Number 6) (Pair (Number 7) (Number 7)))) (Pair (Number 6) (Number 6))),
    (Pair (Pair (Pair (Pair (Number 3) (Number 0)) (Pair (Number 5) (Number 6))) (Number 1)) (Pair (Pair (Pair (Number 9) (Number 3)) (Pair (Number 1) (Number 7))) (Pair (Pair (Number 3) (Number 4)) (Pair (Number 2) (Number 7)))))
    ]
