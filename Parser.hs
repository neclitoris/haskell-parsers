{-#
    LANGUAGE
    TupleSections,
    FunctionalDependencies,
    FlexibleInstances,
    FlexibleContexts
#-}

module Parser
    ( ParserC
    , runParserC
    , Parser(..)
    , string
    , pEOF
    , module Control.Applicative
    , module Control.Monad
    , module Data.List
    )
where

import           Token
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Monoid


-- we should be able to construct parsers matching a certain symbol
class Parser parser sym tok | parser sym -> tok where
    symbol :: sym -> parser tok
    symbolIs :: (sym -> Bool) -> parser tok

class ParserEOF parser where
    pEOF :: parser ()


-- Simple concrete parser type. Takes enclosing monad, token type
-- and result type as parameters.
newtype ParserC s m t a = ParserC { runParserC :: s -> m (a, s) }

instance Monad m => Functor (ParserC s m t) where
    -- simply apply f to parsing result
    fmap f (ParserC x) = ParserC $ \s -> do
        (a, s') <- x s
        return (f a, s')

instance Monad m => Applicative (ParserC s m t) where
    -- parse nothing, return m
    pure m = ParserC $ \s -> return (m, s)
    -- run first parser and apply function it yields to the result of second parser
    ParserC f <*> ParserC g = ParserC $ \s -> do
        (fun, s' ) <- f s
        (x  , s'') <- g s'
        return (fun x, s'')

instance MonadPlus m => Alternative (ParserC s m t) where
    -- failed parse
    empty = ParserC $ const empty
    -- choose a successfull parse from two options
    -- for Maybe this will yield first (Just x) of arguments
    -- for List this will concatenate lists (yielding all possible parses so far)
    ParserC f <|> ParserC g = ParserC $ \s -> f s <|> g s

instance Monad m => Monad (ParserC s m t) where
    -- run ma, then apply f to the result and use resulting parser for further parsing
    -- this allows to parse non-context-free languages
    ParserC ma >>= f = ParserC $ \s -> do
        (a, s') <- ma s
        runParserC (f a) s'


instance ( Token symbol token
         , Stream state token
         , MonadPlus monad )
         => Parser (ParserC state monad token) symbol token where
    symbolIs p = ParserC $ \str -> case suncons str of
        Just (c, str') | symIs p c -> return (c, str')
        _                          -> empty
    symbol c = symbolIs (== c)

instance ( Stream state token
         , MonadPlus monad ) => ParserEOF (ParserC state monad token) where
    pEOF = ParserC $ \str -> if eof str then return ((), str) else empty


string :: (Token sym t, Parser p sym t, Applicative p) => [sym] -> p [t]
string = sequenceA . map symbol
