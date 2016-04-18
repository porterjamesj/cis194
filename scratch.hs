-- empty :: [a] -> [a]
-- empty _ = []

-- weird :: [a] -> [a]
-- weird [] = []
-- weird (x:xs) = [x]


-- f1 :: Maybe a -> [Maybe a]
-- f1 m = [m,m]

-- f2 :: Maybe a -> [a]
-- f2 Nothing = []
-- f2 (Just x) = [x]

-- the robber problem

-- recursive edition (exponential)

maxMoney :: [Int] -> Int
naiveMaxMoney (x:xs) = max robIt skipIt
  where
    robIt = x + (naiveMaxMoney $ drop 1 xs)
    skipIt = naiveMaxMoney xs
naiveMaxMoney [] = 0

-- dynamic programming edition (linear)
