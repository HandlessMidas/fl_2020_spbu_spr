module Expr where

import           AST                 (AST (..), Operator (..), Subst (..))
import           Combinators         (Parser (..), Result (..), fail',
                                      runParser, satisfy, stream, success, symbol, string)
import           Control.Applicative
import           Data.Char           (digitToInt, isDigit, isLetter)
import qualified Data.Map            as Map

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary

evalExpr :: Subst -> AST -> Maybe Int
evalExpr _ (Num x)        = Just x
evalExpr s (Ident x)      = Map.lookup x s
evalExpr s (UnaryOp op x) = do
                              x' <- evalExpr s x
                              return $ compute $ UnaryOp op (Num x')
evalExpr s (BinOp op l r) = do
                              l' <- evalExpr s l
                              r' <- evalExpr s r
                              return $ compute $ BinOp op (Num l') (Num r')

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr [] ep _ _                   = ep
uberExpr ((op, assoc):pars) ep bin un =
  case assoc of
    Unary -> un <$> op <*> lp <|> lp
    Binary NoAssoc    -> do
        l <- lp
        opp <- op
        r <- lp
        return $ bin opp l r
      <|> lp
    Binary LeftAssoc  -> do
        (f, s) <- (,) <$> lp <*> (many ((,) <$> op <*> lp))
        return $ foldl (\l (opp, r) -> bin opp l r) f s
      <|> lp
    Binary RightAssoc -> do
        (f, s) <- (,) <$> (many ((,) <$> lp <*> op)) <*> lp
        return $ foldr (\(r, op) l -> bin op r l) s f
      <|> lp
    where lp = uberExpr pars ep bin un


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

parseNegNum :: Parser String String Int
parseNegNum = error "parseNegNum undefined"

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
    Success rest ast | null (stream rest) -> return $ compute ast
    _                                     -> Nothing

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
compute (UnaryOp Minus x) = - compute x
compute (UnaryOp Not x) = case (compute x) of
                             p | p == 0 -> 1
                             p          -> 0    
