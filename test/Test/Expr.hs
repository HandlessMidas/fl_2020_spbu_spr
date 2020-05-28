module Test.Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser,
                                      symbol, string)
import           Control.Applicative ((<|>))
import           Expr                (Associativity (..), evaluate, parseExpr,
                                      parseNum, parseOp, toOperator, uberExpr, parseIdent)
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)

isFailure (Failure _) = True
isFailure  _          = False

unit_evaluate :: Assertion
unit_evaluate = do
  evaluate ("2*5+x*0+y") @?= Just (BinOp Plus (Num 10) (Ident "y"))
  evaluate ("2*(5+x)*0+y") @?= Just (Ident "y")
  evaluate ("100*c+(20*b+4*c)*0+58*c+0") @?= Just (BinOp Plus (BinOp Mult (Num 100) (Ident "c")) (BinOp Mult (Num 58) (Ident "c")))
  evaluate ("(98+a)+4*b*0+(4+c)*b") @?= Just (BinOp Plus (BinOp Plus (Num 98) (Ident "a")) (BinOp Mult (BinOp Plus (Num 4) (Ident "c")) (Ident "b")))
  evaluate ("a*1+2*a+(0*b+8)*9") @?= 
    Just (BinOp Plus (BinOp Plus (Ident "a") (BinOp Mult (Num 2) (Ident "a"))) (Num 72))