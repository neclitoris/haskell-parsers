module Parser
    ( Parser(..)
    , parse
    , parseIf
    , parseEOF
    , module Control.Applicative
    , module Control.Monad
    , module Data.List
    )
where

import           Control.Applicative
import           Control.Monad
import           Data.List

-- Parser type. Takes enclosing monad, token type and result type as parameters.
-- As abstract as it gets, I guess.
newtype Parser m s a = Parser { runParser :: [s] -> m (a, [s]) }

instance Monad m => Functor (Parser m s) where
    -- simply apply f to parsing result
    fmap f (Parser x) = Parser $ \s -> do
        (a, s') <- x s
        return (f a, s')

instance Monad m => Applicative (Parser m s) where
    -- parse nothing, return m
    pure m = Parser $ \s -> return (m, s)
    -- run first parser and apply function it yields to the result of second parser
    Parser f <*> Parser g = Parser $ \s -> do
        (fun, s' ) <- f s
        (x  , s'') <- g s'
        return (fun x, s'')

instance MonadPlus m => Alternative (Parser m s) where
    -- failed parse
    empty = Parser $ const empty
    -- choose a successfull parse from two options
    -- for Maybe this will yield first (Just x) of arguments
    -- for List this will concatenate lists (yielding all possible parses so far)
    Parser f <|> Parser g = Parser $ \s -> f s <|> g s

instance Monad m => Monad (Parser m s) where
    -- run ma, then apply f to the result and use resulting parser for further parsing
    -- this allows to parse non-context-free languages
    Parser ma >>= f = Parser $ \s -> do
        (a, s') <- ma s
        runParser (f a) s'

instance MonadPlus m => MonadPlus (Parser m s)

-- consume single character
-- naming is difficult, don't @ me
parse :: MonadPlus m => Parser m s s
parse = Parser $ \str -> maybe empty return $ uncons str

-- consume character if it satisfies the predicate
parseIf :: MonadPlus m => (s -> Bool) -> Parser m s s
parseIf p = Parser $ \str -> case str of
    c : str' | p c -> return (c, str')
    _              -> empty

-- assert that there is no more input
parseEOF :: MonadPlus m => Parser m s ()
parseEOF = Parser $ \str -> case str of
    [] -> return ((), [])
    _  -> empty
