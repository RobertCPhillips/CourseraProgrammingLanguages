--hw 9

import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
         deriving Show
         
----------------------------------------------------
--ex0
----------------------------------------------------
natToInteger1 :: Nat -> Integer
natToInteger1 Zero = 0
natToInteger1 (Succ n) = natToInteger1 n + 1

natToInteger2 :: Nat -> Integer
natToInteger2 (Succ n) = natToInteger1 n + 1
natToInteger2 Zero = 0

--natToInteger3 :: Nat -> Integer
--natToInteger3 n = natToInteger3 n

natToInteger4 :: Nat -> Integer
natToInteger4 (Succ n) = 1 + natToInteger4 n
natToInteger4 Zero = 0

-- natToInteger5 :: Nat -> Integer
-- natToInteger5 Zero = 1
-- natToInteger5 (Succ n) = (1 + natToInteger5 n) - 1

natToInteger6 :: Nat -> Integer
natToInteger6 = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1:m n)]]
      
natToInteger7 :: Nat -> Integer
natToInteger7 = \ n -> genericLength [c | c <- show n,c == 'S']

--natToInteger8 :: Nat -> Integer
--natToInteger8 = \ n -> length [c | c <- show n,c == 'S']

----------------------------------------------------
--ex1
----------------------------------------------------
integerToNat1 0 = Zero
integerToNat1 (n+1) = Succ (integerToNat1 n)

-- integerToNat2 0 = Succ Zero
-- integerToNat2 n = (Succ (integerToNat2 n))

--integerToNat3 n = product [(unsafeCoerce c) :: Integer | c <- show n]

--integerToNat4 n = integerToNat4 n

integerToNat5 (n+1) = Succ (integerToNat5 n)
integerToNat5 0 = Zero

integerToNat6 (n+1) = let m = integerToNat6 n in Succ m
integerToNat6 0 = Zero

-- integerToNat7 = head . m
  -- where {
         -- ; m 0 = [0]
         -- ; m (n+1) = [sum [x | x <- (1:m n)]]
        -- }

-- integerToNat8 :: Integer -> Nat
-- integerToNat8 = \ n -> genericLength [c | c <- show n, isDigit c]

----------------------------------------------------
--ex2
----------------------------------------------------
add1 Zero n = n
add1 (Succ m) n = Succ (add1 n m)

add2 (Succ m) n = Succ (add2 n m)
add2 Zero n = n

-- add3 Zero n = Zero
-- add3 (Succ m) n = Succ (add3 m n)

-- add4 (Succ m) n = Succ (add4 m n)
-- add4 Zero n = Zero

-- add5 n Zero = Zero
-- add5 n (Succ m) = Succ (add5 n m)

-- add6 n (Succ m) = Succ (add6 n m)
-- add6 n Zero = Zero

add7 n Zero = n
add7 n (Succ m) = Succ (add7 m n)

add8 n (Succ m) = Succ (add8 m n)
add8 n Zero = n

----------------------------------------------------
--ex3
----------------------------------------------------
-- mult1 Zero Zero = Zero
-- mult1 m (Succ n) = add1 m (mult1 m n)

mult2 m Zero = Zero
mult2 m (Succ n) = add1 m (mult2 m n)

-- mult3 m Zero = Zero
-- mult3 m (Succ n) = add1 n (mult3 m n)

-- mult4 m Zero = Zero
-- mult4 m n = add1 m (mult4 m (Succ n))

----------------------------------------------------
--ex4
----------------------------------------------------
data Tree = Leaf Integer
          | Node Tree Integer Tree
          
occurs1 :: Integer -> Tree -> Bool
occurs1 m (Leaf n) = m == n
occurs1 m (Node l n r)
  = case compare m n of 
         LT -> occurs1 m l
         EQ -> True
         GT -> occurs1 m r

-- occurs2 :: Integer -> Tree -> Bool
-- occurs2 m (Leaf n ) = m == n
-- occurs2 m (Node l n r)
  -- = case compare m n of 
         -- LT -> occurs2 m r
         -- EQ -> True
         -- GT -> occurs2 m l


-- occurs3 :: Integer -> Tree -> Bool
-- occurs3 m (Leaf n ) = compare m n
-- occurs3 m (Node l n r)
  -- = case compare m n of 
         -- LT -> occurs3 m l
         -- EQ -> True
         -- GT -> occurs3 m r

          
-- occurs4 :: Integer -> Tree -> Bool
-- occurs4 m (Leaf n) = m == n
-- occurs4 m (Node l n r)
  -- = case compare m n of 
         -- LT -> occurs4 m l
         -- EQ -> False
         -- GT -> occurs4 m r

occurs5 :: Integer -> Tree -> Bool
occurs5 m (Leaf n) = m == n
occurs5 m (Node l n r)
  | m == n = True
  | m < n = occurs5 m l
  | otherwise = occurs5 m r

-- occurs6 :: Integer -> Tree -> Bool
-- occurs6 m (Leaf n) = m == n
-- occurs6 m (Node l n r)
  -- | m == n = True
  -- | m > n = occurs6 m l
  -- | otherwise = occurs6 m r

-- occurs7 :: Integer -> Tree -> Bool
-- occurs7 m n = m == n
-- occurs7 m (Node l n r)
  -- | m == n = True
  -- | m < n = occurs7 m l
  -- | otherwise = occurs7 m r

-- occurs8 :: Integer -> Tree -> Bool
-- occurs8 m n = m == n
-- occurs8 m (Node l n r)
  -- | m == n = False
  -- | m < n = occurs8 m r
  -- | otherwise = occurs8 m l


----------------------------------------------------
--ex5
----------------------------------------------------
data Tree2 = Leaf2 Integer
           | Node2 Tree2 Tree2

-- leaves1 (Leaf2 x) = x
-- leaves1 (Node2 l r) = leaves1 l + leaves1 r
-- balanced1 (Leaf2 _) = True
-- balanced1 (Node2 l r)
  -- = abs (leaves1 - leaves1 r) <= 1 || balanced1 l || balanced1 r

leaves2 (Leaf2 x) = x
leaves2 (Node2 l r) = leaves2 l + leaves2 r
balanced2 (Leaf2 _) = True
balanced2 (Node2 l r)
  = abs (leaves2 l - leaves2 r) <= 1

-- leaves3 (Leaf2 x) = x
-- leaves3 (Node2 l r) = leaves3 l + leaves3 r
-- balanced3 (Leaf2 _) = True
-- balanced3 (Node2 l r)
  -- = abs (leaves3 l + leaves3 r) <= 1

leaves4 (Leaf2 x) = x
leaves4 (Node2 l r) = leaves4 l + leaves4 r
balanced4 (Leaf2 _) = True
balanced4 (Node2 l r)
  = abs (leaves4 l - leaves4 r) <= 1 && balanced4 l && balanced4 r

----------------------------------------------------
--ex6
----------------------------------------------------
halve xs = splitAt (length xs `div` 2) xs

balance1 :: [Integer] -> Tree2
balance1 [x] = Leaf2 x
balance1 xs = Node2 (balance1 ys) (balance1 zs)
  where (ys, zs) = halve xs

-- halve2 xs = splitAt (length xs / 2) xs
-- balance2 :: [Integer] -> Tree2
-- balance2 [x] = Leaf2 x
-- balance2 xs = Node2 (balance2 ys) (balance2 zs)
  -- where (ys, zs) = halve2 xs

-- balance3 :: [Integer] -> Tree2
-- balance3 [x] = Leaf2 x
-- balance3 xs = Node2 ys zs
  -- where (ys, zs) = balance3 (halve xs)

-- balance4 :: [Integer] -> Tree2
-- balance4 x = Leaf2 x
-- balance4 xs = Node2 (balance4 ys) (balance4 zs)
  -- where (ys, zs) = halve xs

----------------------------------------------------
--ex7
----------------------------------------------------
data Expr7 = Add Expr7 Expr7
           | Val Int

----------------------------------------------------
--ex10
----------------------------------------------------

instance Monad [] where
         return x = [x]
         xs >>= f = f xs









