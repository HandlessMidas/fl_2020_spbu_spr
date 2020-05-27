module AST where

import           Text.Printf (printf)

data Operator = Plus
              | Mult
              deriving (Eq)

data AST = BinOp Operator AST AST
         | Ident String
         | Num  Int
         deriving (Eq)

instance Show Operator where
  show Plus   = "+"
  show Mult   = "*"

instance Show AST where
  show = printf "%s" . format
    where
      format t =
        case t of
          BinOp op l r -> printf "(%s%s%s)" (format l) (show op) (format r)
          Ident x -> x
          Num i -> show i

