module Int where

import Nat (Nat, (*), (+))
import Prelude hiding (Int, (*), (+), (-))

-- | ||Tipos|||
data Int where
  MkInt :: (Nat, Nat) -> Int
  deriving (Show, Eq)

-- | ||Operações|||
(+) :: Int -> Int -> Int
(+) (MkInt (a, b)) (MkInt (c, d)) = MkInt (a Nat.+ c, b Nat.+ d)

(-) :: Int -> Int
(-) (MkInt (a, b)) = MkInt (b, a)

(*) :: Int -> Int -> Int
(*) (MkInt (a, b)) (MkInt (c, d)) = MkInt ((a Nat.* c) Nat.+ (b Nat.* d), (a Nat.* d) Nat.+ (b Nat.* c))

-- O resto que eventualmente apareça