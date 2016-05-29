whatFloor :: String -> Int -> Int
whatFloor (p:rest) n | p == '(' = whatFloor rest n+1
                     | p == ')' = whatFloor rest n-1
                     | otherwise = n
whatFloor [] n = n

whatPosition :: String -> Int -> Int -> Int
whatPosition (p:rest) flr now = if newFlr == (-1) then now
                                                  else 1 + whatPosition rest newFlr now
                                where newFlr = whatFloor [p] flr

