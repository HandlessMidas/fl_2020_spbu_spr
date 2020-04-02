module Test.LLang where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser,
                                      symbol)
import           Control.Applicative ((<|>))
import           Expr                (Associativity (..), evaluate, parseExpr,
                                      parseNum, parseOp, toOperator, uberExpr, parseIdent)

import           LLang               (LAst (..), parseBrackets, parseIf, parseWhile, parseAssign,
                                      parseRead, parseWrite, parseSeq, parseInstr, parseL)

import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)

isFailure (Failure _) = True
isFailure  _          = False

unit_parseBrackets :: Assertion
unit_parseBrackets = do
    runParser parseBrackets "(1)" @?= Success "" (Num 1)
    runParser parseBrackets "  \n  \n\n  ( \n\n\n  Expr   \n\n  \n) \n \n " @?= Success " \n \n " (Ident "Expr" )
    assertBool "" $ isFailure (runParser parseBrackets "(fail")

unit_parseIf :: Assertion
unit_parseIf = do
    runParser parseIf "if (x==1) { write(1); write(x); } else {  write(2);}" @?= Success "" 
        (If (BinOp Equal (Ident "x") (Num 1)) (Seq [Write (Num 1), Write (Ident "x")]) (Seq [Write (Num 2)]))
    runParser parseIf "if (x==1) { write(1); write(x); } else {}" @?= Success "" 
        (If (BinOp Equal (Ident "x") (Num 1)) (Seq [Write (Num 1), Write (Ident "x")]) (Seq []))
    assertBool "" $ isFailure (runParser parseIf "if () { write(1); } else { write(2); }")
    assertBool "" $ isFailure (runParser parseIf "if (x == 1) { write(x); }")
    assertBool "" $ isFailure (runParser parseIf "if (x == 1) { write(x); } else")
    assertBool "" $ isFailure (runParser parseIf "if () { write(x); } { write(1); }")
    assertBool "" $ isFailure (runParser parseIf "write(1)")

unit_parseWhile :: Assertion
unit_parseWhile = do
    runParser parseWhile "while (x==1) { write(1); write(x); }" @?= Success "" 
        (While (BinOp Equal (Ident "x") (Num 1)) (Seq [Write (Num 1), Write (Ident "x")]))
    runParser parseWhile "while (x==1) {}" @?= Success "" 
        (While (BinOp Equal (Ident "x") (Num 1)) (Seq []))
    assertBool "" $ isFailure (runParser parseWhile "(1)")
    assertBool "" $ isFailure (runParser parseWhile "while () { write(1); }")

unit_parseAssign :: Assertion
unit_parseAssign = do
    runParser parseAssign "assign _ (1)" @?= Success "" 
        (Assign "_" (Num 1))
    runParser parseAssign "assign   \n \n  abc123'' \n    ( 1+22 )" @?= Success "" 
        (Assign "abc123''" (BinOp Plus (Num 1) (Num 22)))
    assertBool "" $ isFailure (runParser parseAssign "assign 1 _")
    assertBool "" $ isFailure (runParser parseAssign "assign _ 1")
    assertBool "" $ isFailure (runParser parseAssign "assign 1 (_)")
    assertBool "" $ isFailure (runParser parseAssign "write (1)")

unit_parseRead :: Assertion
unit_parseRead = do
    runParser parseRead "read (_)" @?= Success "" 
        (Read "_")
    runParser parseRead "read  \n (abc123'')" @?= Success "" 
        (Read "abc123''")
    assertBool "" $ isFailure (runParser parseRead "read 0")
    assertBool "" $ isFailure (runParser parseRead "write(1)")

unit_parseWrite :: Assertion
unit_parseWrite = do
    runParser parseWrite "write (_)" @?= Success "" 
        (Write (Ident "_"))
    runParser parseWrite "write     \n  \n\n (abc123''+1)" @?= Success "" 
        (Write (BinOp Plus (Ident "abc123''") (Num 1)))
    assertBool "" $ isFailure (runParser parseWrite "write 1")
    assertBool "" $ isFailure (runParser parseWrite "write x")
    assertBool "" $ isFailure (runParser parseWrite "read (_)")

unit_parseSeq :: Assertion
unit_parseSeq = do
    runParser parseSeq "{   \n\n\n   write(1);  \n  read(x);   read(y); \n}" @?= Success "" 
        (Seq [Write (Num 1), Read "x", Read "y"])
    runParser parseSeq "{write(1);}" @?= Success "" 
        (Seq [Write (Num 1)])
    runParser parseSeq "{ \n \n \n } \n" @?= Success " \n" 
        (Seq [])
    assertBool "" $ isFailure (runParser parseSeq "{ write(1) }")
    assertBool "" $ isFailure (runParser parseSeq "{ write(1) read(2); }")
    assertBool "" $ isFailure (runParser parseSeq "{ write(1); read(2) }")
    assertBool "" $ isFailure (runParser parseSeq "write(1)")


unit_parseL :: Assertion
unit_parseL = do
    runParser parseL "{ read(x); if (1==x) { write(1); } else { write(2); }; read(y); read(x); }"
        @?= Success "" (Seq [
                             Read "x",
                             If (BinOp Equal (Num 1) (Ident "x")) (Seq [Write (Num 1)]) (Seq [Write (Num 2)]),
                             Read "y",
                             Read "x"
                            ])    
    assertBool "" $ isFailure (runParser parseL "read x")
    assertBool "" $ isFailure (runParser parseL "assign x (5)")
    assertBool "" $ isFailure (runParser parseL "if (_) {write (1);} else { write (2); }")
    assertBool "" $ isFailure (runParser parseL "while (_) { write (1); }")
    assertBool "" $ isFailure (runParser parseL "write(1)")