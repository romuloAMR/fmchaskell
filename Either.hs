module Either where

data Either a b = Left a | Right b
  deriving ( Eq , Show )