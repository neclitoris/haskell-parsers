{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}

module Parser
  ( ParserC,
    runParserC,
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


-- Simple concrete parser type. Takes enclosing monad, token type
-- and result type as parameters.
newtype ParserC s m a = ParserC {runParserC :: s -> m [(a, s)]}

instance Functor m => Functor (ParserC s m) where
  -- simply apply f to parsing result
  fmap f (ParserC xs) = ParserC (fmap (\l -> [(f a, s') | (a, s') <- l]) . xs)

instance Monad m => Applicative (ParserC s m) where
  -- parse nothing, return m
  pure m = ParserC $ \s -> pure [(m, s)]

  -- run first parser and apply function it yields to the result of second parser
  ParserC f <*> ParserC g = ParserC $ \s -> do
    xs <- f s
    (fmap join . sequenceA) [let a' (x, s) = (a x, s) in fmap (fmap a') (g s') | (a, s') <- xs]

instance Monad m => Alternative (ParserC s m) where
  -- failed parse
  empty = ParserC $ const $ pure []

  -- choose a successfull parse from two options
  -- for Maybe this will yield first (Just x) of arguments
  -- for List this will concatenate lists (yielding all possible parses so far)
  ParserC f <|> ParserC g = ParserC $ \s -> (++) <$> f s <*> g s

instance Monad m => Monad (ParserC s m) where
  -- run ma, then apply f to the result and use resulting parser for further parsing
  -- this allows to parse non-context-free languages
  ParserC ma >>= f = ParserC $ \s -> do
    l <- ma s
    (fmap join . sequenceA) $ [runParserC (f a) s' | (a, s') <- l]

symbolIs :: (Token sym t, Stream s t, Applicative m) => (sym -> Bool) -> ParserC s m t
symbolIs p = ParserC $ \str -> case suncons str of
  Just (c, str') | symIs p c -> pure [(c, str')]
  _ -> pure []

symbol :: (Token sym t, Stream s t, Applicative m) => sym -> ParserC s m t
symbol c = symbolIs (== c)

pEOF :: (Stream s t, Applicative m) => ParserC s m ()
pEOF = ParserC $ \str -> if eof str then pure [((), str)] else pure []

string :: (Token sym t, Stream s t, Monad m) => [sym] -> ParserC s m [t]
string = traverse symbol
