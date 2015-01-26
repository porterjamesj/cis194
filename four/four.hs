fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\x -> if even x
                                                            then x `div` 2
                                                            else 3 * x + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)


height :: Tree a -> Integer
height (Node i _ _ _) = i
height Leaf = -1  -- this is clearly a hack but it works

insert :: a -> Tree a -> Tree a
insert x Leaf = (Node 0 Leaf x Leaf)
insert x (Node i left x' right)
  | hl > hr = (Node i left x' (insert x right))
  | hr > hl = (Node i (insert x left) x' right)
  | otherwise = (Node (futureh+1) (insert x left) x' right)
  where
    hl = height left
    hr = height right
    futureh = height (insert x left)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xorOnce :: Bool -> Bool -> Bool
xorOnce p q = (p || q) && (not (p && q))

xor :: [Bool] -> Bool
xor = foldl xorOnce False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- this is obviously not what was intended but nobody forbade reverse!
myFoldl f base xs = foldr (flip f) base (reverse xs)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

toDelete :: Integer -> [Integer]
toDelete n = filter (<= n) $ map (\(i,j) -> i + j + 2*i*j) $ filter (uncurry (<)) $ cartProd [1..n] [1..n]

valid x = not $ elem x $ map (\(i,j) -> i + j + 2*i*j) $ filter (uncurry (<)) (cartProd [1..x] [1..x])

sieveSundaram :: Integer -> [Integer]
sieveSundaram = (map (\x -> 2*x+1)) . (filter valid) . (enumFromTo 1)
