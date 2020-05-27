module Main where

import           Combinators (runParser, Result (..))
import           Expr        (evaluate, parseExpr)
import           Text.Printf (printf)

main :: IO ()
main = do
    a <- getLine    
    putStrLn $ (maybe "error" show . evaluate $ a)