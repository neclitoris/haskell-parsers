module Main where

import           Parser
import           System.IO
import           Data.Char
import           Data.Foldable
import           System.Environment

type ExprParser a = Parser Maybe Char a

stoi :: String -> Integer
stoi s = foldl' (\acc c -> acc * 10 + (toInteger . digitToInt) c) 0 s

skipWS :: ExprParser ()
skipWS = void $ many $ parseIf isSpace

parseEnd :: (Char -> Bool) -> ExprParser ()
parseEnd p = (void $ parseIf p) <|> parseEOF

number :: ExprParser Integer
number =
    skipWS
        *> (   (negate <$ parseIf (== '-') <|> pure id)
           <*> (stoi <$> some (parseIf isDigit))
           )

operation
    :: Char
    -> (Integer -> Integer -> Integer)
    -> ExprParser (Integer -> Integer -> Integer)
operation c f = skipWS *> parseIf (== c) *> pure f

operationsL
    :: ExprParser (Integer -> Integer -> Integer)
    -> ExprParser Integer
    -> ExprParser Integer
operationsL op val = foldl' (flip ($)) <$> val <*> many (flip <$> op <*> val)

operationsR
    :: ExprParser (Integer -> Integer -> Integer)
    -> ExprParser Integer
    -> ExprParser Integer
operationsR op val = flip (foldr' ($)) <$> many (val <**> op) <*> val

atom :: ExprParser Integer
atom = skipWS *> parseIf (== '(') *> parseExpr <* parseIf (== ')') <|> number

additiveOps = operationsL $ operation '+' (+) <|> operation '-' (-)
multiplicativeOps = operationsL $ operation '*' (*)
powerOps = operationsR $ operation '^' (^)
allOps = [additiveOps, multiplicativeOps, powerOps]

parseExpr :: ExprParser Integer
parseExpr = foldr' ($) atom allOps

main :: IO ()
main = do
    (str : _) <- getArgs
    let Just (res, "") = runParser (parseExpr <* skipWS <* parseEOF) str
    print res
