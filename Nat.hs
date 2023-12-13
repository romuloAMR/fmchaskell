module Nat where

import Bool (Bool (True, False), myIf, not, (&&), (||))
import Either (Either (Left, Right), unwrap)
import Error (Error (DivZero, ListEmpty, NoHead, NoInit, NoLast, NoTail))
import Prelude hiding (Bool (True, False), Either, Left, Right, div, gcd, max, min, not, pred, quot, rem, (&&), (*), (+), (-), (/=), (<), (<=), (==), (>), (>=), (^), (||), lcm)

data Nat = O | S Nat
  deriving (Eq, Show)

-- | ||Operações|||
(+) :: Nat -> Nat -> Nat
(+) O n = n
(+) (S m) n = S (m + n)

(*) :: Nat -> Nat -> Nat
(*) O n = O
(*) (S m) n = n + (m * n)

(^) :: Nat -> Nat -> Nat
(^) n O = S O
(^) n (S m) = n * (n ^ m)

(-) :: Nat -> Nat -> Nat
(-) O m = O
(-) n O = n
(-) (S n) (S m) = n - m

(==) :: Nat -> Nat -> Bool
(==) O O = True
(==) O _ = False
(==) _ O = False
(==) (S n) (S m) = n == m

(/=) :: Nat -> Nat -> Bool
(/=) n m = not (n == m)

(<) :: Nat -> Nat -> Bool
(<) O (S _) = True
(<) (S n) (S m) = n < m
(<) _ _ = False

(<=) :: Nat -> Nat -> Bool
(<=) n m = (n < m) || (n == m)

(>) :: Nat -> Nat -> Bool
(>) n m = m < n

(>=) :: Nat -> Nat -> Bool
(>=) n m = m <= n

-- | ||Funções|||
pred :: Nat -> Nat
pred n = n - S O

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

fib :: Nat -> Nat
fib O = S O
fib (S O) = S O
fib (S (S n)) = fib (S n) + fib n

min :: Nat -> Nat -> Nat
min _ O = O
min O _ = O
min (S n) (S m) = S (min n m)

max :: Nat -> Nat -> Nat
max m O = m
max O n = n
max (S n) (S m) = S (max n m)

div :: Nat -> Nat -> Either Error (Nat, Nat)
div n O = Left DivZero
div n m = Right (unwrap (quot n m), rem n m)

quot :: Nat -> Nat -> Either Error Nat
quot n O = Left DivZero
quot n (S O) = Right n
quot O m = Right O
quot n m = Right (S (unwrap (quot (n - m) m)))

rem :: Nat -> Nat -> Nat
rem n O = n
rem O m = O
rem (S n) (S m) = myIf (n < m) (S n) (rem (S n - S m) (S m))

gcd :: Nat -> Nat -> Either Error Nat
gcd n O = Left DivZero
gcd O m = Right m
gcd (S n) (S m) = gcd (S m) (rem (S n) (S m))

lcm :: Nat -> Nat -> Nat
lcm n m = unwrap (quot (n * m) (unwrap (gcd n m)))

ev :: Nat -> Bool
ev n = rem n (S (S O)) == O

od :: Nat -> Bool
od n = rem n (S (S O)) == S O

isMul₃ :: Nat -> Bool
isMul₃ = divides (S (S (S O)))

divides :: Nat -> Nat -> Bool
divides n m = rem m n == O

isZero :: Nat -> Bool
isZero O = True
isZero n = False