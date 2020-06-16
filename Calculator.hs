module Calculator where

import           Parser
import           System.IO
import           Data.Char
import           Data.Foldable
import           System.Environment

-- Not going to use any kind of error reporting
type ExprParser a = Parser Maybe Char a

stoi :: String -> Integer
stoi s = foldl' (\acc c -> acc * 10 + (toInteger . digitToInt) c) 0 s

skipWS :: ExprParser ()
skipWS = void $ many $ parseCharIf isSpace

-- drop whitespaces, negate parse result if '-' is present
-- some (parseCharIf isDigit) yields a list of one or more digits, see Alternative typeclass
number :: ExprParser Integer
number =
    skipWS
        *> (   (negate <$ parseChar '-' <|> pure id)
           <*> (stoi <$> some (parseCharIf isDigit))
           )

operation
    :: String
    -> (Integer -> Integer -> Integer)
    -> ExprParser (Integer -> Integer -> Integer)
operation s f = skipWS *> parseString s *> pure f

-- chain any number of left-associative operations
-- foldl' (flip ($)) is lifted to act on Parsers
-- then its arguments are parsed: an integer and a list of functions
-- first, a single val is parsed, then any number of strings " + x"
-- operand parser passed as a parameter because there is a certain trick, bear with me
operationsL
    :: ExprParser (Integer -> Integer -> Integer)
    -> ExprParser Integer
    -> ExprParser Integer
operationsL op val = foldl' (flip ($)) <$> val <*> many (flip <$> op <*> val)

-- same but for right associative operations
-- notice the <**>, it's semantically a flipped <*>
-- (not the same as flip (<*>)! that would parse right operand first)
-- this is needed so that val is parsed before op
operationsR
    :: ExprParser (Integer -> Integer -> Integer)
    -> ExprParser Integer
    -> ExprParser Integer
operationsR op val = flip (foldr' ($)) <$> many (val <**> op) <*> val

-- now define the grammar
atom :: ExprParser Integer
atom = skipWS *> parseChar '(' *> parseExpr <* parseChar ')' <|> number

-- a bunch of operations are defined and arranged in a list according to their priorities
additiveOps = operationsL $ operation "+" (+) <|> operation "-" (-)
multiplicativeOps = operationsL $ operation "*" (*)
powerOps = operationsR $ operation "^" (^)
allOps = [additiveOps, multiplicativeOps, powerOps]

-- this thing expands into (additiveOps $ (multiplicativeOps $ (powerOps $ atom)))
-- that was the trick with operator chaining function taking operand parser as a param
parseExpr :: ExprParser Integer
parseExpr = foldr' ($) atom allOps

main :: IO ()
main = do
    (str : _) <- getArgs
    let Just (res, "") = runParser (parseExpr <* skipWS <* parseEOF) str
    print res
