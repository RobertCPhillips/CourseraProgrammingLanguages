
--------------------------------------------------
--e1 all :: (a -> Bool) -> [a] -> Bool
--------------------------------------------------
all1 p xs = and (map p xs)
--all2 p xs = map p (and xs)
all3 p = and . map p
all4 p = not . any (not . p)
--all5 p = map p . and
all6 p xs = foldl (&&) True (map p xs)
all7 p xs = foldr (&&) False (map p xs)
all8 p = foldr (&&) True . map p

--------------------------------------------------
--e2 any :: (a -> Bool) -> [a] -> Bool
--------------------------------------------------
--any1 p = map p . or
any2 p = or . map p
any3 p xs = length (filter p xs) > 0
any4 p = not . null . dropWhile (not . p)
any5 p = null . filter p
any6 p xs = not (all (\x -> not (p x)) xs)
any7 p xs = foldr (\x acc -> (p x) || acc) False xs
any8 p xs = foldr (||) True (map p xs)

--------------------------------------------------
--e3 takeWhile :: (a -> Bool) -> [a] -> [a]
--------------------------------------------------
takeWhile1 _ [] = []
takeWhile1 p (x:xs)
  | p x = x : takeWhile1 p xs
  | otherwise = takeWhile1 p xs
  
takeWhile2 _ [] = []
takeWhile2 p (x:xs)
  | p x = x : takeWhile2 p xs
  | otherwise = []
  
takeWhile3 _ [] = []
takeWhile3 p (x:xs)
  | p x = takeWhile3 p xs
  | otherwise = []
  
takeWhile4 p = foldl (\ acc x -> if p x then x : acc else acc) []

--------------------------------------------------
--e4 dropWhile :: (a -> Bool) -> [a] -> [a]
--------------------------------------------------
dropWhile1 _ [] = []
dropWhile1 p (x:xs)
  | p x = dropWhile1 p xs
  | otherwise = x:xs
  
dropWhile2 _ [] = []
dropWhile2 p (x:xs)
  | p x = dropWhile2 p xs
  | otherwise = xs

dropWhile3 p = foldr (\ x acc -> if p x then acc else x:acc) []

--------------------------------------------------
--e5 map :: (a -> b) -> [a] -> [b]
--------------------------------------------------
map1 f = foldr (\ x xs -> xs ++ [f x]) []
map2 f = foldr (\ x xs -> f x ++ xs) []
map3 f = foldl (\ xs x -> f x : xs) []
map4 f = foldl (\ xs x -> xs ++ [f x]) []

--------------------------------------------------
--e6 filter :: (a -> Bool) -> [a] -> [b]
--------------------------------------------------
filter1 p = foldl (\ xs x -> if p x then x : xs else xs) []
filter2 p = foldr (\ x xs -> if p x then x : xs else xs) []
filter3 p = foldr (\ x xs -> if p x then xs ++ [x] else xs) []
--filter4 p = foldl (\ x xs -> if p x then xs ++ [x] else xs) []

--------------------------------------------------
--e7 dec2int :: [Integer] -> Integer
--------------------------------------------------
dec2int1 = foldr (\x y -> 10 * x + y) 0
dec2int2 = foldl (\x y -> x + 10 * y) 0
dec2int3 = foldl (\x y -> 10 * x + y) 0
dec2int4 = foldr (\x y -> x + 10 * y) 0

--------------------------------------------------
--e8
--------------------------------------------------
--sumsqreven1 = compose1 [sum, map (^2), filter even]
--sumsqreven2 = compose1 [sum, product]

compose1 :: [a -> a] -> (a -> a)
compose1 = foldr (.) id

--------------------------------------------------
--e9 - curry :: ((a,b) -> c) -> a -> b -> c
--------------------------------------------------
e9 (x,y) = x+y

curry1 f = \ x y -> f x y
curry2 f = \ x y -> f
curry3 f = \ x y -> f (x,y)
curry4 f = \ (x,y) -> f x y

--------------------------------------------------
--e10 - uncurry :: (a -> b -> c) -> (a,b) -> c
--------------------------------------------------
e10 x y = x + y

uncurry1 f = \ (x,y) -> f x y
uncurry2 f = \ x y -> f (x,y)
uncurry3 f = \ (x,y) -> f
uncurry4 f = \ x y -> f

--------------------------------------------------
--e13
--------------------------------------------------
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)
  
iterate1 f = unfold (const False) id f
iterate2 f = unfold (const False) f f
iterate3 f = unfold (const True) id f
iterate4 f = unfold (const True) f f








