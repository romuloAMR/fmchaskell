module Types (Empty, Day, Pair, Maybe) where

import Prelude hiding (Either, Just, Left, Maybe, Nothing, Right)

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show)

data Empty

data Pair a = MkPair a a
  deriving (Eq, Show)

data Maybe a = Nothing | Just a
  deriving (Eq, Show)