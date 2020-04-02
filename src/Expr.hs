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

data OpType = Binary Associativity
            | Unary

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr [] ep _ _             = ep
uberExpr ((op, assoc):xs) ep bin un = 
  case assoc of
    Unary            -> un <$> op <*> lp <|> lp
    Binary LeftAssoc -> do
      first <- lp
      rest <- many $ (flip (,)) <$> op <*> lp
      return $ foldl (flip $ uncurry $ flip $ flip . bin) first rest  
    Binary RightAssoc -> do
      rest <- many $ (,) <$> lp <*> op
      first <- lp
      return $ foldr (uncurry $ flip bin) first rest
    Binary NoAssoc    -> (flip bin <$> lp <*> op <*> lp) <|> lp
  where lp = uberExpr xs ep bin un


-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [(parseOp' "||", Binary RightAssoc),
                     (parseOp' "&&", Binary RightAssoc),
                     (parseOp' "!", Unary),
                     (parseOp' "==" <|> parseOp' "/=" <|> parseOp' "<=" <|> parseOp' "<" <|> parseOp' ">=" <|> parseOp' ">", Binary NoAssoc),
                     (parseOp' "+" <|> parseOp' "-", Binary LeftAssoc),
                     (parseOp' "*" <|> parseOp' "/", Binary LeftAssoc),
                     (parseOp' "-", Unary),
                     (parseOp' "^", Binary RightAssoc)]
                     (Num <$> parseNum <|> Ident <$> parseIdent <|> symbol '(' *> parseExpr <* symbol ')')
                     BinOp
                     UnaryOp

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 `fmap` go
  where
    go :: Parser String String String
    go = some (satisfy isDigit)

parseIdent :: Parser String String String
parseIdent = do
  b <- some ((satisfy isLetter) <|> (symbol '_'))
  m <- many ((satisfy isLetter) <|> (symbol '_') <|> (satisfy isDigit))
  e <- many (symbol '\'')
  return $ b ++ m ++ e

parseString :: [String] -> Parser String String String
parseString [s]    = (string s)
parseString (x:xs) = (string x) <|> parseString xs

operators = ["+", "-", "*", "/=", "/", "==", "=", "<=", ">=", "<", ">", "||", "&&", "^", "!"]

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = (parseString operators) >>= toOperator

parseOp' :: String -> Parser String String Operator
parseOp' op = string op >>= toOperator

parseSpaces :: Parser String String String
parseSpaces = many $ symbol ' ' <|> symbol '\n'

parseAccurate :: String -> Parser String String String
parseAccurate = foldr (\fst rest -> fmap (:) (symbol fst) <*> rest) $ pure ""

-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+"  = success Plus
toOperator "*"  = success Mult
toOperator "-"  = success Minus
toOperator "/"  = success Div
toOperator "^"  = success Pow
toOperator "||" = success Or
toOperator "&&" = success And
toOperator "==" = success Equal
toOperator "/=" = success Nequal
toOperator "<=" = success Le
toOperator "<"  = success Lt
toOperator ">=" = success Ge
toOperator ">"  = success Gt
toOperator "!"  = success Not
toOperator _    = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing

boolToInt:: Bool -> Int
boolToInt True = 1
boolToInt _    = 0

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y
compute (BinOp Pow x y)   = (compute x) ^ (compute y)
compute (BinOp Or  x y)   = case (compute x) of
                               p | p == 0 -> compute y
                               p          -> p
compute (BinOp And x y)   = case (compute x) of
                               p | p /= 0 -> compute y
                               p          -> p
compute (BinOp Equal x y) = boolToInt $ (compute x) == (compute y)
compute (BinOp Nequal x y) = boolToInt $ (compute x) /= (compute y)
compute (BinOp Le     x y) = boolToInt $ (compute x) <= (compute y)
compute (BinOp Lt     x y) = boolToInt $ (compute x) < (compute y)
compute (BinOp Ge     x y) = boolToInt $ (compute x) >= (compute y)
compute (BinOp Gt     x y) = boolToInt $ (compute x) > (compute y) 
