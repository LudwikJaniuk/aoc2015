{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Attoparsec.Number
import qualified Data.ByteString.Lazy as C
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

main = do
    inp <- C.getContents
    print $ (numOf . filterRed . fromJust) (decode $ inp :: Maybe Value) -- $ numOf $ fromJust $ (decode $ inp :: Maybe Value)

fromJust (Just x) = x

filterRed :: Value -> Value
filterRed (Object o) = if any isRed (values o)
                       then Null
                       else Object $ M.map filterRed o
    where isRed = (== (String "red"))
filterRed (Array a) = Array $ V.map filterRed a
filterRed x = x

numOf :: Value -> Number
numOf (Number n) = n
numOf (Object o) = sum $ values $ M.map numOf o
numOf (Array a) = V.foldl' (+) 0 $ V.map numOf a
numOf _ = 0

values = snd . unzip . M.toList