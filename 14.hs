import Data.List
import Data.Matrix as M
import qualified Data.Vector as V

type Reindeer = (Int, Int, Int)

main = do
  input <- getContents
  print $ race input

race = V.maximum . score . map parseRecord . lines

raceLength = 2503

--points :: [Reindeer] -> [Int]
--points rs = zipSum pointsAwarded where
  --pointsAwarded = map (awardPoints . distancesCovered

score :: [Reindeer] -> V.Vector Int
score = zipSum . V.map givePoint . vecVecs . getMatrixAsVector . M.transpose . distanceMatrix

vecVecs :: V.Vector Int -> V.Vector (V.Vector Int)
vecVecs xs = chunksOf (V.length xs `div` raceLength) xs

chunksOf :: Int -> V.Vector a -> V.Vector (V.Vector a)
chunksOf n xs | V.null xs = V.empty
              | otherwise = V.cons firstChunk (chunksOf n rest) where
                 (firstChunk, rest) = V.splitAt n xs

givePoint :: V.Vector Int -> V.Vector Int
givePoint ps = V.map pointGiver ps where
    pointGiver p = if p == V.maximum ps then 1 else 0

zipSum :: V.Vector (V.Vector Int) -> V.Vector Int
zipSum xs = V.foldl' zipSum2 (V.replicate innerSize 0) xs where
    innerSize = V.length (V.last xs)
    zipSum2 :: V.Vector Int -> V.Vector Int -> V.Vector Int
    zipSum2 as bs = V.map sumPair $ V.zip as bs
    sumPair (a,b) = a+b

distanceMatrix :: [Reindeer] -> Matrix Int
distanceMatrix = fromLists . map distances

distances :: Reindeer -> [Int]
distances r = map (flip simulate r) [1..raceLength]

simulate :: Int -> Reindeer -> Int
simulate steps (speed, fly, rest) = foldl' (+) 0 $ take steps derivatives where
  derivatives :: [Int]
  derivatives = cycle (replicate fly speed ++ replicate rest 0)

parseRecord str = (speed, fly, rest) where
  nums = map (read :: String -> Int) $ words str
  speed = nums !! 3
  fly = nums !! 6
  rest = nums !! 13
