--week 8 hw

------------------------------------
--q1
--Takes a String as its parameter and writes it to the standard output
------------------------------------
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

------------------------------------
--q2
------------------------------------
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""

------------------------------------
--q3
------------------------------------
getLine' :: IO String
getLine' = get []

get :: String -> IO String
get xs
 = do x <- getChar
      case x of 
           '\n' -> return xs
           _ -> get (xs ++ [x])

------------------------------------
--q4
------------------------------------
interact' f
  = do input <- getLine'
       putStrLn' (f input)

------------------------------------
--q5
------------------------------------
sequence2_' [] = return ()
sequence2_' (m:ms) = (foldl (>>) m ms) >> return ()

sequence4_' [] = return ()
sequence4_' (m:ms) = m >> sequence4_' ms

sequence5_' [] = return ()
sequence5_' (m:ms) = m >>= \ _ -> sequence5_' ms

sequence7_' ms = foldr (>>) (return ()) ms

------------------------------------
--q6
------------------------------------
sequence1' [] = return []
sequence1' (m:ms)
  = m >>=
     \ a ->
       do as <- sequence1' ms
          return (a:as)

sequence5' ms = foldr func (return []) ms
  where
         func :: (Monad m) => m a -> m [a] -> m [a]
         func m acc 
           = do x <- m
                xs <- acc
                return (x:xs)                
         
sequence8' [] = return []
sequence8' (m:ms) 
  = do a <- m
       as <- sequence8' ms
       return (a:as)

------------------------------------
--q7
------------------------------------
mapM1' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM1' f as = sequence1' (map f as)

mapM2' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM2' f [] = return []
mapM2' f (a:as) 
  = f a >>= \ b -> mapM2' f as >>= \bs -> return (b:bs)

--mapM3' :: Monad m => (a -> m b) -> [a] -> m [b]  
--mapM3' f as = sequence2_' (map f as)

-- mapM4' f [] = return []
-- mapM4' f (a:as)
  -- = f a >> \ b -> mapM4' f as >> \ bs -> return (b:bs)

-- mapM5' f [] = return []
-- mapM5' f (a:as) =
  -- do
      -- f a -> b
      -- mapM5' f as -> bs
      -- return (b:bs)

mapM6' :: Monad m => (a -> m b) -> [a] -> m [b]      
mapM6' f [] = return []
mapM6' f (a:as)
  = do b <- f a
       bs <- mapM6' f as
       return (b:bs)       

mapM7' :: Monad m => (a -> m b) -> [a] -> m [b]       
mapM7' f [] = return []
mapM7' f (a:as)
  = f a >>=
   \ b ->
     do bs <- mapM7' f as
        return (b:bs)
        
mapM8' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM8' f [] = return []
mapM8' f (a:as)
  = f a >>=
    \ b ->
      do bs <- mapM8' f as
         return (bs ++ [b])

------------------------------------
--q8
------------------------------------
filterM1' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM1' _ [] = return []
filterM1' p (x:xs)
  = do flag <- p x
       ys <- filterM1' p xs
       return (x:xs)

filterM2' :: Monad m => (a -> m Bool) -> [a] -> m [a]       
filterM2' _ [] = return []
filterM2' p (x:xs)
  = do flag <- p x
       ys <- filterM2' p xs
       if flag then return (x:ys) else return ys

-- filterM3' :: Monad m => (a -> m Bool) -> [a] -> m [a]       
-- filterM3' _ [] = return []
-- filterM3' p (x:xs)
  -- = do ys <- filterM3' p xs
       -- if p x then return (x:ys) else return ys

filterM4' :: Monad m => (a -> m Bool) -> [a] -> m [a]       
filterM4' _ [] = return []
filterM4' p (x:xs)
  = do flag <- p x
       ys <- filterM4' p xs
       if flag then return ys else return (x:ys)

------------------------------------
--q9
------------------------------------





















