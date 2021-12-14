module Day14 where

import qualified Data.Map as M
import Data.Maybe

type Pairs = M.Map String Char
type Template = M.Map String Int

-- PART 1
step :: Pairs -> Template -> Template
step pairs template = foldl f M.empty $ M.toList template
    where
    f acc ([a,b], n) = M.insertWith (+) [c,b] n $ M.insertWith (+) [a,c] n acc
        where
            c = fromJust $ M.lookup [a,b] pairs
    f _ _ = undefined

counts :: Char -> Template -> M.Map Char Int
counts firstChar = M.insertWith (+) firstChar 1
    . foldl (\acc ([_, b], n) -> M.insertWith (+) b n acc) M.empty
    . M.toList

stringToTemplate :: String -> Template
stringToTemplate s = foldl (\acc (a,b) -> M.insertWith (+) [a,b] 1 acc) M.empty $ zip s (drop 1 s)

score :: String -> Int -> Int
score s n = maximum m - minimum m
    where
    m = counts (head s) $ iterate (step input) (stringToTemplate s) !! n

day14p1 = score "CVKKFSSNNHNPSPPKBHPB" 10

-- PART 2
day14p2 = score "CVKKFSSNNHNPSPPKBHPB" 40

-- INPUT
test = M.fromList [
    ("CH",'B'),
    ("HH",'N'),
    ("CB",'H'),
    ("NH",'C'),
    ("HB",'C'),
    ("HC",'B'),
    ("HN",'C'),
    ("NN",'C'),
    ("BH",'H'),
    ("NC",'B'),
    ("NB",'B'),
    ("BN",'B'),
    ("BB",'N'),
    ("BC",'B'),
    ("CC",'N'),
    ("CN",'C')
    ]

input = M.fromList [
    ("OF",'S'),
    ("VO",'F'),
    ("BP",'S'),
    ("FC",'S'),
    ("PN",'K'),
    ("HC",'P'),
    ("PP",'N'),
    ("FK",'V'),
    ("KN",'C'),
    ("BO",'O'),
    ("KS",'B'),
    ("FF",'S'),
    ("KC",'B'),
    ("FV",'C'),
    ("VF",'N'),
    ("HS",'H'),
    ("OS",'F'),
    ("VC",'S'),
    ("VP",'P'),
    ("BC",'O'),
    ("HF",'F'),
    ("HO",'F'),
    ("PC",'B'),
    ("CC",'K'),
    ("NB",'N'),
    ("KK",'N'),
    ("KP",'V'),
    ("BH",'H'),
    ("BF",'O'),
    ("OB",'F'),
    ("VK",'P'),
    ("FB",'O'),
    ("NP",'B'),
    ("CB",'C'),
    ("PS",'S'),
    ("KO",'V'),
    ("SP",'C'),
    ("BK",'O'),
    ("NN",'O'),
    ("OC",'F'),
    ("VB",'B'),
    ("ON",'K'),
    ("NK",'B'),
    ("CK",'H'),
    ("NH",'N'),
    ("CV",'C'),
    ("PF",'P'),
    ("PV",'V'),
    ("CP",'N'),
    ("FP",'N'),
    ("SB",'B'),
    ("SN",'N'),
    ("KF",'F'),
    ("HP",'S'),
    ("BN",'V'),
    ("NF",'B'),
    ("PO",'O'),
    ("CH",'O'),
    ("VV",'S'),
    ("OV",'V'),
    ("SF",'P'),
    ("BV",'S'),
    ("FH",'V'),
    ("CN",'H'),
    ("VH",'V'),
    ("HB",'B'),
    ("FN",'P'),
    ("OH",'S'),
    ("SK",'H'),
    ("OP",'H'),
    ("VN",'V'),
    ("HN",'P'),
    ("BS",'S'),
    ("CF",'B'),
    ("PB",'H'),
    ("SS",'K'),
    ("NV",'P'),
    ("FS",'N'),
    ("CS",'O'),
    ("OK",'B'),
    ("CO",'O'),
    ("VS",'F'),
    ("OO",'B'),
    ("NO",'H'),
    ("SO",'F'),
    ("HH",'K'),
    ("FO",'H'),
    ("SH",'O'),
    ("HV",'B'),
    ("SV",'N'),
    ("PH",'F'),
    ("BB",'P'),
    ("KV",'B'),
    ("KB",'H'),
    ("KH",'N'),
    ("NC",'P'),
    ("SC",'S'),
    ("PK",'B'),
    ("NS",'V'),
    ("HK",'B')
    ]