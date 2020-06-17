{-#
    LANGUAGE
    FunctionalDependencies,
    FlexibleInstances
#-}

module Token
    ( Token(..)
    , Stream(..)
    , Numbered(..)
    )
where

import           Data.List

-- after tokenization process you get a stream of tokens,
-- each describing a certain terminal symbol and possibly
-- carrying some addtl info like line number
class Eq sym => Token sym tok where
    sym :: sym -> tok -> Bool
    symIs :: (sym -> Bool) -> tok -> Bool

    sym c = symIs (== c)

-- stream allows us to get tokens one by one and may eventually
-- end
-- suncons stands for 'stream uncons'
class Stream str tok | str -> tok where
    suncons :: str -> Maybe (tok, str)
    eof :: str -> Bool


-- having symbol as a token for itself may be useful for simple parsing
-- without error reporting and such
instance Eq sym => Token sym sym where
    symIs = ($)

-- simplest possible stream is list
instance Stream [tok] tok where
    suncons = uncons
    eof     = null


-- introducing token type with additional info here
newtype Numbered a = Numbered (a, Int) deriving Show

-- it encapsulates a token
instance Token s a => Token s (Numbered a) where
    s `sym` Numbered (t, _) = s `sym` t
    s `symIs` Numbered (t, _) = s `symIs` t
