import Data.Char
import Prelude hiding ((!!))

-----------------------
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-----------------------
find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-----------------------
positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where n = length xs - 1
  
-----------------------
let2int :: Char -> Char -> Int
let2int c a = ord c - ord a

int2let :: Int -> Char -> Char
int2let n a = chr ((ord a) + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let (((let2int c 'a') + n) `mod` 26) 'a'
  | isUpper c = int2let (((let2int c 'A') + n) `mod` 26) 'A'
  | otherwise = c
  
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

------------------------
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n-1)

------------------------
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) =
  if x <= y then x : merge xs (y:ys) else y : merge (x : xs) ys

-----------------------
halve xs = splitAt (length xs `div` 2) xs

msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where (ys, zs) = halve xs

