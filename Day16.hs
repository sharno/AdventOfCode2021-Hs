module Day16 where

import Numeric (readHex, readBin)
import Text.Printf (printf)

-- PART 1
hexToBin :: String -> String
hexToBin s =  concat $ map (printf "%04b" . fst . head . (readHex :: ReadS Int) . (:[])) s

binToInt :: String -> Int
binToInt s = fst . head $ readBin s

data Exp 
    = Literal Int Int -- version number
    | Operator Int Int [Exp] -- version operation [exp]
    | ParsingLiteral Int String -- version remaining
    deriving Show

parse :: Int -> [Exp] -> String -> ([Exp], String)
parse count state@(Literal _ _:_) remaining | length state == count = (state, remaining)
parse count state@(Operator _ _ _: _) remaining | length state == count = (state, remaining)

parse count (ParsingLiteral version s:state) ('0':a:b:c:d:remaining)       = parse count (Literal version (binToInt (s ++ [a,b,c,d])):state) remaining
parse count (ParsingLiteral version s:state) ('1':a:b:c:d:remaining)       = parse count (ParsingLiteral version (s ++ [a,b,c,d]):state) remaining
parse count state                            (a:b:c:'1':'0':'0':remaining) = parse count (ParsingLiteral (binToInt [a,b,c]) "":state) remaining

parse count state (a:b:c:d:e:f:'0':remaining) | length remaining >= 15 = parse count (Operator version op (reverse parsed):state) $ drop (15+n) remaining
    where
        version = binToInt [a,b,c]
        op = binToInt [d,e,f]
        n = binToInt $ take 15 remaining
        (parsed, _) = parse (-1) [] $ take n $ drop 15 remaining

parse count state (a:b:c:d:e:f:'1':remaining) | length remaining >= 11 = parse count (Operator version op (reverse parsed):state) rest
    where
        version = binToInt [a,b,c]
        op = binToInt [d,e,f]
        n = binToInt $ take 11 remaining
        (parsed, rest) = parse n [] $ drop 11 remaining

parse _ state rest = (state, rest)


score :: Int -> [Exp] -> Int
score s (Literal v _:rest) = score (s+v) rest
score s (Operator v _ nest:rest) = score (s+v+ score 0 nest) rest
score s _ = s

day16p1 = score 0 $ fst $ parse (-1) [] $ hexToBin input

-- PART 2
score2 :: Exp -> Int
score2 (Literal _ n) = n
score2 (Operator _ 0 xs) = sum     $ map score2 xs
score2 (Operator _ 1 xs) = product $ map score2 xs
score2 (Operator _ 2 xs) = minimum $ map score2 xs
score2 (Operator _ 3 xs) = maximum $ map score2 xs
score2 (Operator _ 5 [a,b]) = if score2 a > score2 b then 1 else 0
score2 (Operator _ 6 [a,b]) = if score2 a < score2 b then 1 else 0
score2 (Operator _ 7 [a,b]) = if score2 a == score2 b then 1 else 0


day16p2 = score2 $ head $ fst $ parse (-1) [] $ hexToBin input


-- INPUT
input = "E20D7880532D4E551A5791BD7B8C964C1548CB3EC1FCA41CC00C6D50024400C202A65C00C20257C008AF70024C00810039C00C3002D400A300258040F200D6040093002CC0084003FA52DB8134DE620EC01DECC4C8A5B55E204B6610189F87BDD3B30052C01493E2DC9F1724B3C1F8DC801E249E8D66C564715589BCCF08B23CA1A00039D35FD6AC5727801500260B8801F253D467BFF99C40182004223B4458D2600E42C82D07CC01D83F0521C180273D5C8EE802B29F7C9DA1DCACD1D802469FF57558D6A65372113005E4DB25CF8C0209B329D0D996C92605009A637D299AEF06622CE4F1D7560141A52BC6D91C73CD732153BF862F39BA49E6BA8C438C010E009AA6B75EF7EE53BBAC244933A48600B025AD7C074FEB901599A49808008398142013426BD06FA00D540010C87F0CA29880370E21D42294A6E3BCF0A080324A006824E3FCBE4A782E7F356A5006A587A56D3699CF2F4FD6DF60862600BF802F25B4E96BDD26049802333EB7DDB401795FC36BD26A860094E176006A0200FC4B8790B4001098A50A61748D2DEDDF4C6200F4B6FE1F1665BED44015ACC055802B23BD87C8EF61E600B4D6BAD5800AA4E5C8672E4E401D0CC89F802D298F6A317894C7B518BE4772013C2803710004261EC318B800084C7288509E56FD6430052482340128FB37286F9194EE3D31FA43BACAF2802B12A7B83E4017E4E755E801A2942A9FCE757093005A6D1F803561007A17C3B8EE0008442085D1E8C0109E3BC00CDE4BFED737A90DC97FDAE6F521B97B4619BE17CC01D94489E1C9623000F924A7C8C77EA61E6679F7398159DE7D84C015A0040670765D5A52D060200C92801CA8A531194E98DA3CCF8C8C017C00416703665A2141008CF34EF8019A080390962841C1007217C5587E60164F81C9A5CE0E4AA549223002E32BDCEA36B2E100A160008747D8B705C001098DB13A388803F1AE304600"

