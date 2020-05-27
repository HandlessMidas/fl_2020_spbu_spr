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