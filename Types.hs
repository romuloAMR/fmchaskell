module Types (Empty, Day, Pair, Maybe) where
import Prelude hiding(Maybe, Either, Left, Right, Nothing, Just)

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving ( Show )

data Empty

data Pair a = MkPair a a
  deriving ( Eq , Show )

data Maybe a = Nothing | Just a
  deriving ( Eq , Show )