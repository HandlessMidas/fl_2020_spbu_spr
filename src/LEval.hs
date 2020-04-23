module LEval where

import LLang (Program (..), Configuration (..), parseProg, eval, Function (..))
import qualified Data.Map    as Map
import Combinators (Result (..), InputStream (..), runParser)

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg (Program functions main) inp = eval main $ Conf Map.empty inp [] $ Map.fromList $ fmap (\f -> (name f, f)) functions

parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg prog inp = case (runParser parseProg prog) of
    Success (InputStream stream curPos) result -> evalProg result inp
    Failure _                                  -> Nothing