import Data.List

type HappyIndex = [((String, String), Int)]

main = do
  input <- getContents
  let inData = map parseRecord $ lines input
  print $ ultimateHappiness $ addMe inData

addMe :: HappyIndex -> HappyIndex
addMe inData = meToOthers ++ othersToMe ++ inData where
  meToOthers = [((me, other), 0) | other <- names']
  othersToMe = [((other, me), 0) | other <- names']
  me = "Me"
  names' = names inData


ultimateHappiness :: [((String, String), Int)] -> Int
ultimateHappiness inData = maximum $ map totalHappiness seatings where
  seatings = permutations $ names inData
  totalHappiness seating = sum $ map happiness $ neighbors seating
  happiness (n1, p, n2) = happinessGain (p, n1) + happinessGain (p, n2)
  happinessGain k = case lookup k inData of Just n -> n

names = foldl' (flip $ insert . fst . fst) [] where
  insert x xs = if x `elem` xs then xs else (x:xs)

neighbors :: [a] -> [(a,a,a)]
neighbors list = take (length list) $ zip3 inf (drop 1 inf) (drop 2 inf) where
  inf = cycle list

parseRecord str = ((who, whom), howMuch) where
  words' = words str
  who = head words'
  whom = init $ last words'
  howMuch = case words' !! 2 of 
            "gain" -> totalHappiness
            "lose" -> (-totalHappiness)
  totalHappiness = read $ words' !! 3


