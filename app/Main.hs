module Main where

import Data.String.Strip
import Text.Parsec

data Expr = Volume Integer
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Divide Expr Expr
          | Modular Expr Expr
          deriving Show

str = "13%7-2*3-(1+2)+6/2"

number = do
  x <- many1 digit
  return $ Volume (read x :: Integer)

expr = do
  x <- term
  fs <- many $ do char '+'
                  a <- term
                  return $ (flip Plus) a
           <|> do char '-'
                  b <- term
                  return $ (flip Minus) b
  return $ foldr (\f x -> f x) x fs

term = do
  x <- parens
  fs <- many $ do char '*'
                  a <- parens
                  return $ (flip Times) a
           <|> do char '/'
                  b <- parens
                  return $ (flip Divide) b
           <|> do char '%'
                  b <- parens
                  return $ (flip Modular) b
  return $ foldr (\f x -> f x) x fs

parens = do x <- number
            return x
     <|> do char '('
            y <- expr
            char ')'
            return y

calc :: Expr -> Integer
calc (Volume x) = x
calc (Plus x y) = (calc x) + (calc y)
calc (Minus x y) = (calc x) - (calc y)
calc (Times x y) = (calc x) * (calc y)
calc (Divide x y) = (calc x) `div` (calc y)
calc (Modular x y) = (calc x) `mod` (calc y)

main = let Right x = parse expr "" str
       in do print $ x
             print $ calc x

