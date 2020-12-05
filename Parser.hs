{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}

module Parser
  ( ParserC,
    runParserC,
    Parser (..),
    string,
    pEOF,
    module Control.Applicative,
    module Control.Monad,
    module Data.List,
  )
where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid
import Token

-- we should be able to construct parsers matching a certain symbol
class Parser parser sym tok | parser sym -> tok where
  symbol :: sym -> parser tok
  symbolIs :: (sym -> Bool) -> parser tok

class ParserEOF parser where
  pEOF :: parser ()

-- Simple concrete parser type. Takes enclosing monad, token type
-- and result type as parameters.
newtype ParserC s m t a = ParserC {runParserC :: s -> m [(a, s)]}

instance Functor m => Functor (ParserC s m t) where
  -- simply apply f to parsing result
  fmap f (ParserC xs) = ParserC (fmap (\l -> [(f a, s') | (a, s') <- l]) . xs)

instance Monad m => Applicative (ParserC s m t) where
  -- parse nothing, return m
  pure m = ParserC $ \s -> pure [(m, s)]

  -- run first parser and apply function it yields to the result of second parser
  ParserC f <*> ParserC g = ParserC $ \s -> do
    xs <- f s
    (fmap join . sequenceA) [let a' (x, s) = (a x, s) in fmap (fmap a') (g s') | (a, s') <- xs]

instance Monad m => Alternative (ParserC s m t) where
  -- failed parse
  empty = ParserC $ const $ pure []

  -- choose a successfull parse from two options
  -- for Maybe this will yield first (Just x) of arguments
  -- for List this will concatenate lists (yielding all possible parses so far)
  ParserC f <|> ParserC g = ParserC $ \s -> (++) <$> f s <*> g s

instance Monad m => Monad (ParserC s m t) where
  -- run ma, then apply f to the result and use resulting parser for further parsing
  -- this allows to parse non-context-free languages
  ParserC ma >>= f = ParserC $ \s -> do
    l <- ma s
    (fmap join . sequenceA) $ [runParserC (f a) s' | (a, s') <- l]

instance
  ( Token symbol token,
    Stream state token,
    Applicative monad
  ) =>
  Parser (ParserC state monad token) symbol token
  where
  symbolIs p = ParserC $ \str -> case suncons str of
    Just (c, str') | symIs p c -> pure [(c, str')]
    _ -> pure []
  symbol c = symbolIs (== c)

instance
  ( Stream state token,
    Applicative monad
  ) =>
  ParserEOF (ParserC state monad token)
  where
  pEOF = ParserC $ \str -> if eof str then pure [((), str)] else pure []

string :: (Token sym t, Parser p sym t, Applicative p) => [sym] -> p [t]
string = traverse symbol
