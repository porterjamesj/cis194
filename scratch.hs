empty :: [a] -> [a]
empty _ = []

weird :: [a] -> [a]
weird [] = []
weird (x:xs) = [x]
