{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

data ObjTree = Leaf String | Node [ObjTree] deriving Show

main = do
    input <- getContents
    print $ sumNums input -- $ (unparseTree . parseTree) input

sumNums = sum . map read . numsOnly

numsOnly = filter isNumStr . groupBy (\a b -> isNum a && isNum b)
    where isNum = flip elem "1234567890-"
          isNumStr = isNum . head



parseTree :: String -> ObjTree
parseTree "" = Leaf ""
parseTree str = let
    preBrace = takeWhile notBrace str
    afterBrace = reverse $ sTail $ dropWhile notBrace str
    postBrace = reverse $ takeWhile notCBrace afterBrace
    inBrace = reverse $ sTail $ dropWhile notCBrace afterBrace
    sTail [] = []
    sTail xs = tail xs
    notBrace = (/= '{')
    notCBrace = (/= '}')
    recurse = Node (Leaf preBrace:parseTree inBrace:Leaf postBrace:[])
    leaf = Leaf str
    in if null inBrace
       then leaf
       else recurse

unparseTree :: ObjTree -> String
unparseTree (Leaf str) = str
unparseTree (Node xs) = "{" ++ (concat . intersperse "," . map unparseTree) xs ++ "}"

filterRed :: ObjTree -> ObjTree
filterRed (Node xs) = if any isRed xs
                      then Node []
                      else Node $ map filterRed xs
    where isRed (Leaf str) = ":\"red\"" `isInfixOf` str
          isRed other = False
filterRed other = other

--111754
test = decode "{\"q\":\"we\"}" :: Maybe Value

