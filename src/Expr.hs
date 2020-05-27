module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', fail',
                              satisfy, success, symbol, string)
import           Data.Char   (digitToInt, isDigit, isLetter)
import           Control.Applicative

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

-- Универсальный парсер выражений
uberExpr :: Monoid e
         => [(Parser e i op, Associativity)] -- список парсеров бинарных операторов с ассоциативностями в порядке повышения приоритета
         -> Parser e i ast -- парсер для элементарного выражения
         -> (op -> ast -> ast -> ast) -- функция для создания абстрактного синтаксического дерева для бинарного оператора
         -> Parser e i ast
uberExpr [] ep _               = ep
uberExpr ((op, assoc):xs) ep f = 
  case assoc of
    LeftAssoc -> do
      first <- lp
      rest <- many $ (flip (,)) <$> op <*> lp
      return $ foldl (flip $ uncurry $ flip $ flip . f) first rest  
    RightAssoc -> do
      rest <- many $ (,) <$> lp <*> op
      first <- lp
      return $ foldr (uncurry $ flip f) first rest
    NoAssoc -> (flip f <$> lp <*> op <*> lp) <|> lp
  where lp = uberExpr xs ep f

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [(parseOp' "+", LeftAssoc),
                     (parseOp' "*", LeftAssoc)]
                     (Num <$> parseNum <|> Ident <$> parseIdent <|> symbol '(' *> parseExpr <* symbol ')')
                     BinOp

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> f acc d) 0 `fmap` go
  where
    go :: Parser String String String
    go = do 
      t <- many (symbol '-')
      s <- some (satisfy isDigit)
      return $ s ++ t
    f acc '-' = -acc
    f acc n   = 10 * acc + (digitToInt n)

parseIdent :: Parser String String String
parseIdent = do
  b <- some ((satisfy isLetter) <|> (symbol '_'))
  m <- many ((satisfy isLetter) <|> (symbol '_') <|> (satisfy isDigit))
  e <- many (symbol '\'')
  return $ b ++ m ++ e

parseString :: [String] -> Parser String String String
parseString [s]    = (string s)
parseString (x:xs) = (string x) <|> parseString xs

operators = ["+", "*"]

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = (parseString operators) >>= toOperator

parseOp' :: String -> Parser String String Operator
parseOp' op = string op >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+"  = success Plus
toOperator "*"  = success Mult

evaluate :: String -> Maybe(AST)
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ optimise ast
    _                            -> Nothing

optimise :: AST -> AST
optimise (Ident x)      = Ident x
optimise (Num x)        = Num x
optimise (BinOp op x y) = case (op, x', y') of
  (Mult, _, Num 0)     -> Num 0
  (Mult, Num 0, _)     -> Num 0
  (Mult, Num a, Num b) -> Num (a * b)
  (Plus, Num 0, b)     -> b
  (Plus, a, Num 0)     -> a
  (Plus, Num a, Num b) -> Num (a + b)
  (op, x, y)           -> BinOp op x y  
  where 
    x' = optimise x
    y' = optimise y
