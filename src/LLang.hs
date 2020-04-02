module LLang where

import AST (AST (..), Operator (..))
import Combinators (Parser (..))
import Expr (parseExpr, parseNum, parseIdent, parseOp, parseAccurate, parseSpaces)
import Control.Applicative

type Expr = AST

type Var = String

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

parseBrackets :: Parser String String AST
parseBrackets = do
  parseSpaces
  parseAccurate "("
  parseSpaces
  expr <- parseExpr
  parseSpaces
  parseAccurate ")"
  return expr


parseIf :: Parser String String LAst
parseIf = do
  parseAccurate "if"
  cond <- parseBrackets
  parseSpaces
  seq1 <- parseSeq
  parseSpaces
  parseAccurate "else"
  parseSpaces
  seq2 <- parseSeq
  return $ If cond seq1 seq2

parseWhile :: Parser String String LAst
parseWhile = do
  parseAccurate "while"
  cond <- parseBrackets
  parseSpaces
  seq <- parseSeq
  return $ While cond seq

parseRead :: Parser String String LAst
parseRead = do
  parseAccurate "read"
  parseSpaces
  parseAccurate "("
  parseSpaces   
  var <- parseIdent
  parseSpaces  
  parseAccurate ")"
  return $ Read var

parseWrite :: Parser String String LAst
parseWrite = do
  parseAccurate "write"
  expr <- parseBrackets
  return $ Write expr

parseAssign :: Parser String String LAst
parseAssign = do
  parseAccurate "assign"
  parseSpaces
  var <- parseIdent
  expr <- parseBrackets
  return $ Assign var expr

parseSeq :: Parser String String LAst
parseSeq = do
  parseAccurate "{"
  parseSpaces
  instrs <- many $ parseInstr <* parseSpaces <* parseAccurate ";" <* parseSpaces
  parseAccurate "}"
  return $ Seq instrs

parseInstr :: Parser String String LAst
parseInstr = parseIf <|> parseWhile <|> parseRead <|> parseWrite <|> parseAssign <|> parseSeq

parseL :: Parser String String LAst
parseL = do
  parseSpaces
  x <- parseSeq
  parseSpaces
  return x