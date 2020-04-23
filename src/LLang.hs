module LLang where

import           AST         (AST (..), Operator (..), Subst (..))
import           Combinators (Parser (..))
import           Expr (parseExpr, parseNum, parseIdent, parseOp, parseAccurate, parseSpaces, evalExpr)
import           Control.Applicative
import           Data.List   (intercalate)
import qualified Data.Map    as Map
import           Text.Printf (printf)

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int], defs :: Defs }
                   deriving (Show, Eq)

type Defs = Map.Map String Function

data Program = Program { functions :: [Function], main :: LAst }

data Function = Function { name :: String, args :: [Var], funBody :: LAst, returnExpr :: Expr }
              deriving (Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Eq)

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

parseArgs :: Parser String String [Var]
parseArgs = ((:) <$> (parseIdent) <*> many(parseSpaces *> parseAccurate "," *> parseSpaces *> parseIdent)) <|> (pure [])

parseL :: Parser String String LAst
parseL = do
  parseSpaces
  x <- parseSeq
  parseSpaces
  return x

parseDef :: Parser String String Function
parseDef = do 
  parseAccurate "func"
  parseSpaces
  name <- parseIdent
  parseSpaces
  parseAccurate "("
  parseSpaces
  args <- parseArgs
  parseSpaces
  parseAccurate ")"
  parseSpaces
  body <- parseSeq
  return $ Function name args body


parseProg :: Parser String String Program
parseProg = Parser $ \input -> runParser' parseProg' input where
  parseProg' = do
    funcs <- (many $ parseSpaces *> parseDef)
    main <- parseL
    return $ Program funcs main

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration 
eval (If cond seq1 seq2) config@(Conf subst input output) = do
    resCond <- evalExpr subst cond
    case resCond of
      0 -> eval seq2 config
      _ -> eval seq1 config

eval while@(While cond seq) config@(Conf subst input output) = do
    resCond <- evalExpr subst cond
    case resCond of
      0 -> return config
      _ -> do
        config' <- eval seq config
        eval while config'

eval (Assign name expr) (Conf subst input output) = do
    resExpr <- evalExpr subst expr
    return $ Conf (Map.insert name resExpr subst) input output

eval (Read name) (Conf subst input output) =
  case input of
    (x:rest) -> return $ Conf (Map.insert name x subst) rest output
    _      -> Nothing

eval (Write expr) (Conf subst input output) = do
    resExpr <- evalExpr subst expr
    return $ Conf subst input (resExpr:output)

eval (Seq instr) config =
  case instr of
    []     -> Just config
    (x:rest) -> do
      resInstr <- eval x config
      eval (Seq rest) resInstr

instance Show Function where
  show (Function name args funBody returnExpr) =
    printf "%s(%s) =\n%s\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody) (identation 1 ("return " ++ show returnExpr))

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id