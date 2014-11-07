import Prelude hiding ((&&))

add x y = x + y
add1 y = add 1 y

second xs = head (tail xs)
swap (x, y) = (y, x)
pair x y = (x, y)
double x = x*2
pal xs = reverse xs == xs
twice f x = f (f x)
f xs = take 3 (reverse xs)
addd = \x -> (\y -> x + y)

e13 x y = x / y

------------------------------------------------------
--halve1 xs = (take n xs, drop n xs)
--  where n = length xs / 2
  
halve2 xs = splitAt (length xs `div` 2) xs

halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs
  
halve4 xs = splitAt (length xs `div` 2)

halve5 xs = (take n xs, drop (n+1) xs)
  where n = length xs `div`  2

halve6 xs = splitAt (div (length xs) 2) xs

--halve7 xs = splitAt (length xs / 2) xs

halve8 xs = (take n xs, drop n xs)
  where n = length xs `div`  2
------------------------------------------------------
safetail1 xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_:xs) = xs

safetail3 (_:xs)
  | null xs = []
  | otherwise = tail xs

safetail4 xs
  | null xs = []
  | otherwise = tail xs
  
--safetail5 xs = tail xs
--safetail5 [] = []

safetail6 [] = []
safetail6 xs = tail xs

safetail7 [x] = [x]
safetail7 (_:xs) = xs

safetail8
 = \ xs ->
     case xs of
       [] -> []
       (_:xs) -> xs
------------------------------------------------------

--False || False = False
--False || True = True
--True || False = True
--True || True = True
------------------------------------------------------

--True && True = True
--_ && _ = False

--a && b = if a then if b then True else False else False
--a && b = if not (a) then not (b) else True
--a && b = if a then b
--a && b = if a then if b then False else True else False
--a && b = if a then b else False
--a && b = if b then a else False
-------------------------------------------------------

mult1 x y z = x * y * z
--mult2 x y z = \ x -> (\ y -> (\ z -> x * y * z))
--mult3 = \x -> (x * \y -> (y * \z -> z))
mult4 = \x -> (\y -> (\z -> x * y * z))
mult5 = ((((\x -> \y) -> \z) -> x * y) * z)



































