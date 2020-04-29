
{-# LANGUAGE FlexibleInstances #-}

module Combinators where

import           Control.Applicative
import           Data.List           (nub, sortBy)

data Result error input result
  = Success (InputStream input) result
  | Failure [ErrorMsg error]
  deriving (Eq)

data Position = Position { line :: Int, col :: Int } deriving (Show, Eq, Ord)

newtype Parser error input result
  = Parser { runParser' :: (InputStream input) -> Result error input result }

data InputStream a = InputStream { stream :: a, curPos :: Position }
                   deriving (Show, Eq, Ord)

data ErrorMsg e = ErrorMsg { errors :: [e], pos :: Position }
                deriving (Eq)

makeError e p = ErrorMsg [e] p

initPosition = Position 0 0

runParser :: Parser error input result -> input -> Result error input result
runParser parser input = runParser' parser (InputStream input initPosition)

toStream :: a -> Position -> InputStream a
toStream = InputStream

incrPos :: Char -> InputStream a -> InputStream a
incrPos '\n' (InputStream str (Position line col)) = InputStream str (Position (line + 1) 0)
incrPos '\t' (InputStream str (Position line col)) = InputStream str (Position line (col + 4 - mod col 4))
incrPos _    (InputStream str (Position line col)) = InputStream str (Position line (col + 1))

incrPos' :: Char -> Position -> Position
incrPos' '\n' (Position line col) = Position (line + 1) 0
incrPos' '\t' (Position line col) = Position line (col + 4 - mod col 4)
incrPos' _    (Position line col) = Position line (col + 1)

instance Functor (Parser error input) where
  fmap f p = Parser runParser'' where
    runParser'' input = case (runParser' p input) of
      Failure error         -> Failure error
      Success input' result -> Success input' (f result) 

instance Applicative (Parser error input) where
  pure result = Parser (\input -> Success input result) 
  p1 <*> p2 = Parser runParser'' where
    runParser'' input = case (runParser' p1 input) of
      Failure error          -> Failure error
      Success input' result1 -> case (runParser' p2 input') of
        Failure error           -> Failure error 
        Success input'' result2 -> Success input'' (result1 result2)

instance Monad (Parser error input) where
  return = pure 

  p >>= f = Parser runParser'' where
    runParser'' input = case (runParser' p input) of
      Failure error         -> Failure error
      Success input' result -> (runParser' (f result) input')

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \input -> Failure [makeError mempty (curPos input)]

  Parser a <|> Parser b = Parser $ \input ->
    case a input of
      Success input' r -> Success input' r
      Failure e ->
        case b input of
          Failure e' -> Failure $ mergeErrors e e'
          x          -> x

mergeErrors :: (Monoid e) => [ErrorMsg e] -> [ErrorMsg e] -> [ErrorMsg e]
mergeErrors e e' =
    merge (sortBy sorting e) (sortBy sorting e')
  where
    merge [] s = s
    merge s [] = s
    merge (ErrorMsg e p : xs) (ErrorMsg e' p' : xs') | p == p' = ErrorMsg (e <> e') p : merge xs xs'
    merge (ErrorMsg e p : xs) e'@(ErrorMsg _ p' : _) | p < p' = ErrorMsg e p : merge xs e'
    merge e@(ErrorMsg _ p : _) (ErrorMsg e' p' : xs) | p > p' = ErrorMsg e' p' : merge xs e

    sorting x y = pos x `compare` pos y

infixl 1 <?>
(<?>) :: Monoid error => error -> Parser error input a -> Parser error input a
(<?>) msg (Parser p) = Parser $ \input ->
    case p input of
      Failure err -> Failure $ mergeErrors [makeError msg (maximum $ map pos err)] err
      x -> x

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Char -> Parser String String Char
symbol c = ("Expected symbol: " ++ show c) <?> satisfy (== c)

eof :: Parser String String ()
eof = Parser $ \input -> if null $ stream input then Success input () else Failure [makeError "Not eof" (curPos input)]

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: (Char -> Bool) -> Parser String String Char
satisfy p = Parser $ \(InputStream input pos) ->
  case input of
    (x:xs) | p x -> Success (incrPos x $ InputStream xs pos) x
    input        -> Failure [makeError "Predicate failed" pos]

-- Успешно парсит пустую строку
epsilon :: Parser e i ()
epsilon = success ()

-- Всегда завершается успехом, вход не читает, возвращает данное значение
success :: a -> Parser e i a
success a = Parser $ \input -> Success input a

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' msg = Parser $ \input -> Failure [makeError msg (curPos input)]

word :: String -> Parser String String String
word w = Parser $ \(InputStream input pos) ->
  let (pref, suff) = splitAt (length w) input in
  if pref == w
  then Success (InputStream suff (foldr incrPos' pos (reverse w))) w
  else Failure [makeError ("Expected " ++ show w) pos]

instance Show (ErrorMsg String) where
  show (ErrorMsg e pos) = "at position " ++ show pos ++ ":\n" ++ (unlines $ map ('\t':) (nub e))

instance (Show input, Show result) => Show (Result String input result) where
  show (Failure e) = "Parsing failed\n" ++ unlines (map show e)
  show (Success i r) = "Parsing succeeded!\nResult:\n" ++ show r ++ "\nSuffix:\t" ++ show i

string :: String -> Parser String String String
string [] = success ""
string (x:xs) = do 
  c <- symbol x
  s <- string xs
  return $ c:s