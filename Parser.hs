{-# LANGUAGE FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE TupleSections, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser
  ( Parser(..),
    ParserT,
    FParserT,
    runParserT,
    runFParserT,
    string,
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


class Parser stream tok parser | parser -> stream, stream -> tok where
  symbol :: forall sym. Token sym tok => sym -> parser tok
  symbolIs :: forall sym. Token sym tok => (sym -> Bool) -> parser tok
  pEOF :: parser ()

  symbol c = symbolIs (== c)

string :: (Token sym t, Applicative p, Parser s t p) => [sym] -> p [t]
string = traverse symbol

-- Simple concrete parser type. Takes enclosing monad, token type
-- and result type as parameters.
newtype ParserT m s a = ParserT {runParserT :: s -> m [(a, s)]}

instance Functor m => Functor (ParserT m s) where
  -- simply apply f to parsing result
  fmap f (ParserT xs) = ParserT (fmap (\l -> [(f a, s') | (a, s') <- l]) . xs)

instance Monad m => Applicative (ParserT m s) where
  -- parse nothing, return m
  pure m = ParserT $ \s -> pure [(m, s)]

  -- run first parser and apply function it yields to the result of second parser
  ParserT f <*> ParserT g = ParserT $ \s -> do
    xs <- f s
    (fmap join . sequenceA) [let a' (x, s) = (a x, s) in fmap (fmap a') (g s') | (a, s') <- xs]

instance Monad m => Alternative (ParserT m s) where
  -- failed parse
  empty = ParserT $ const $ pure []

  -- choose a successfull parse from two options
  -- for Maybe this will yield first (Just x) of arguments
  -- for List this will concatenate lists (yielding all possible parses so far)
  ParserT f <|> ParserT g = ParserT $ \s -> (++) <$> f s <*> g s

instance Monad m => Monad (ParserT m s) where
  -- run ma, then apply f to the result and use resulting parser for further parsing
  -- this allows to parse non-context-free languages
  ParserT ma >>= f = ParserT $ \s -> do
    l <- ma s
    (fmap join . sequenceA) $ [runParserT (f a) s' | (a, s') <- l]

instance (Stream s t, Applicative m) => Parser s t (ParserT m s) where
  symbolIs p = ParserT $ \str -> case suncons str of
    Just (c, str') | symIs p c -> pure [(c, str')]
    _ -> pure []

  pEOF = ParserT $ \str -> if eof str then pure [((), str)] else pure []

newtype FParserT m s a = FParserT (ParserT m s a)
  deriving (Functor, Applicative, Alternative, Monad, Parser s t)

runFParserT :: forall t m s a. (Stream s t, Functor m) => FParserT m s a -> s -> m (Maybe a)
runFParserT (FParserT p) s = fmap fst . find (eof . snd) <$> runParserT p s
