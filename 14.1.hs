import Data.List

type Reindeer = (Int, Int, Int)

main = do
  input <- getContents
  print $ race input

race = maximum . map (simulate raceLength . parseRecord) . lines

raceLength = 2503

simulate :: Int -> Reindeer -> Int
simulate steps (speed, fly, rest) = foldl' (+) 0 $ take steps derivatives where
  derivatives :: [Int]
  derivatives = cycle (replicate fly speed ++ replicate rest 0)

parseRecord str = (speed, fly, rest) where
  nums = map (read :: String -> Int) $ words str
  speed = nums !! 3
  fly = nums !! 6
  rest = nums !! 13