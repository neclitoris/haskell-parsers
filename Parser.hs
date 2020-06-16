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

newtype Parser m s a = Parser { runParser :: [s] -> m (a, [s]) }

instance Monad m => Functor (Parser m s) where
    fmap f (Parser x) = Parser $ \s -> do
        (a, s') <- x s
        return (f a, s')

instance Monad m => Applicative (Parser m s) where
    pure m = Parser $ \s -> return (m, s)
    Parser f <*> Parser g = Parser $ \s -> do
        (fun, s' ) <- f s
        (x  , s'') <- g s'
        return (fun x, s'')

instance MonadPlus m => Alternative (Parser m s) where
    empty = Parser $ const empty
    Parser f <|> Parser g = Parser $ \s -> f s <|> g s

instance Monad m => Monad (Parser m s) where
    Parser ma >>= f = Parser $ \s -> do
        (a, s') <- ma s
        runParser (f a) s'

instance MonadPlus m => MonadPlus (Parser m s)

parse :: MonadPlus m => Parser m s s
parse = Parser $ \str -> maybe empty return $ uncons str

parseIf :: MonadPlus m => (s -> Bool) -> Parser m s s
parseIf p = Parser $ \str -> case str of
    c : str' | p c -> return (c, str')
    _              -> empty

parseEOF :: MonadPlus m => Parser m s ()
parseEOF = Parser $ \str -> case str of
    [] -> return ((), [])
    _  -> empty
