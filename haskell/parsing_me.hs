module Parsing where

import Data.Char
import Control.Monad

infixr 5 +++

newtype Parser a              =  P (String -> [(a,String)])

instance Monad Parser where
  return v                   =  P (\inp -> [(v,inp)])
  p >>= f                    =  undefined

instance MonadPlus Parser where
  mzero                      =  P (\inp -> [])
  p `mplus` q                =  P (\inp -> case parse p inp of
                                            []        -> parse q inp
                                            [(v,out)] -> [(v,out)])
                                            
failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                              []     -> []
                                              (x:xs) -> [(x,xs)])
 
parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

(+++)                         :: Parser a -> Parser a -> Parser a
p +++ q                       =  p `mplus` q

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

char                          :: Char -> Parser Char
char x                        =  sat (== x)














