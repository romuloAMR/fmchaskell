module Nat where
import Prelude hiding (Bool, (+), (*), (-), (^), (==), (/=), (<), (||), (&&), not, (>), (<=), (>=), pred, min, max, div, quot, rem, gcd, length, elem, sum, product, (++), reverse, enumFromTo, take, drop, head, tail, init, last, map, filter, all, any, minimum, maximum, replicate, and, or, concat, takeWhile, dropWhile, Either, Left, Right, Maybe, Nothing, Just, (++), Bool(True, False))

import Bool (Bool, not, (||), (&&), Bool(True, False), myIf)
import Error (Error (DivZero, ListEmpty, NoHead, NoTail, NoInit, NoLast))
import Either (Either(Left, Right))

data Nat = O | S Nat
  deriving ( Eq, Show )

data List a = Nil | Cons a (List a)
  deriving ( Eq , Show )

-- |||Operações|||

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

(++) :: List a -> List a -> List a
(++) Nil l = l
(++) (Cons x xs) l = Cons x (xs ++ l)

-- |||Funções|||

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
min (S n) (S m) =  S (min n m)

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
lcm n m = unwrap (quot (n*m) (unwrap(gcd n m))) 

ev :: Nat -> Bool
ev n = rem n (S(S O)) == O

od :: Nat -> Bool
od n = rem n (S(S O)) == S O

isMul₃ :: Nat -> Bool
isMul₃ = divides (S(S(S O)))

divides :: Nat -> Nat -> Bool
divides n m = rem m n == O

isZero :: Nat -> Bool
isZero O = True
isZero n = False

length :: List a -> Nat
length Nil = O
length (Cons n rest) = S (length rest)

elem :: Nat -> List Nat -> Bool
elem n = any (== n)

sum :: List Nat -> Nat
sum = fold (+) O

product :: List Nat -> Nat
product = fold (*) (S O)

and :: List Bool -> Bool
and = fold (&&) True

or :: List Bool -> Bool
or = fold (||) False

concat :: List (List a) -> List a
concat = fold (++) (Nil :: List a)  

reverse :: List Nat -> List Nat
reverse Nil = Nil
reverse (Cons n rest) = reverse rest ++ Cons n Nil

allEven :: List Nat -> Bool
allEven = all ev

anyEven :: List Nat -> Bool
anyEven = any ev

allOdd :: List Nat -> Bool
allOdd = all od

anyOdd :: List Nat -> Bool
anyOdd = any od

allZero :: List Nat -> Bool
allZero = all isZero

anyZero :: List Nat -> Bool
anyZero = any isZero

addNat :: Nat -> List Nat -> List Nat
addNat n = map (+n)

multNat :: Nat -> List Nat -> List Nat
multNat n = map (*n)

expNat :: Nat -> List Nat -> List Nat
expNat n = map (^n)

enumFromTo :: Nat -> Nat -> List Nat
enumFromTo n m = myIf (n > m)  Nil (Cons n (enumFromTo (S n) m))

enumTo :: Nat -> List Nat
enumTo = enumFromTo O

take :: Nat -> List Nat -> List Nat
take _ Nil = Nil
take O l = Nil
take (S n) (Cons m l) = Cons m (take n l)

drop :: Nat -> List Nat -> List Nat
drop _ Nil = Nil
drop O l = l
drop (S n) (Cons m l) = drop n l

-- elemIndices :: Nat -> List Nat -> List Nat

pwAdd :: List Nat -> List Nat -> List Nat
pwAdd = pw (+)

pwMult :: List Nat -> List Nat -> List Nat
pwMult = pw (*)

isSorted :: List Nat -> Bool
isSorted Nil = True
isSorted (Cons _ Nil) = True
isSorted (Cons m (Cons n r)) = myIf (m <= n) (isSorted (Cons n r)) False

filterEven :: List Nat -> List Nat
filterEven = filter ev

filterOdd :: List Nat -> List Nat
filterOdd = filter od

minimum :: List Nat -> Either Error Nat
minimum Nil = Left ListEmpty
minimum (Cons n Nil) = Right n
minimum (Cons m (Cons n r)) = minimum (Cons (min m n) r)

maximum :: List Nat -> Either Error Nat
maximum Nil = Left ListEmpty
maximum (Cons n Nil) = Right n
maximum (Cons m (Cons n r)) = maximum (Cons (max m n) r)

isPrefixOf :: List Nat -> List Nat -> Bool
isPrefixOf Nil Nil = True
isPrefixOf (Cons n r) Nil = False
isPrefixOf Nil (Cons n r) = True
isPrefixOf (Cons n r) (Cons m s) = myIf (n==m) (r `isPrefixOf` s) False

mix :: List Nat -> List Nat -> List Nat
mix Nil s = s
mix r Nil = r
mix (Cons n r) (Cons m s) = Cons n (Cons m (mix r s))

intersperse :: Nat -> List Nat -> List Nat
intersperse n Nil = Nil
intersperse n (Cons m Nil) = Cons m Nil
intersperse n (Cons m r) = Cons m (Cons n (intersperse n r))

head :: List Nat -> Either Error Nat
head Nil = Left NoHead
head (Cons n r) = Right n

tail :: List Nat -> Either Error (List Nat)
tail Nil = Left NoTail
tail (Cons n r) = Right r

init :: List Nat -> Either Error (List Nat)
init Nil = Left NoInit
init (Cons n Nil) = Right Nil
init (Cons n r) = Right (Cons n (unwrap (init r)))

last :: List Nat -> Either Error Nat
last Nil = Left NoLast
last (Cons n Nil) = Right n
last (Cons n r) = last r

replicate :: Nat -> a -> List a
replicate O a = Nil
replicate (S n) a = Cons a (replicate n a)

map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons a r) = Cons (f a) (map f r)

filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons a r) = myIf (p a) (Cons a (filter p r)) (filter p r)

all :: (a -> Bool) -> List a -> Bool
all p Nil = True
all p (Cons n rest) = myIf (p n) (all p rest) False

any :: (a -> Bool) -> List a -> Bool
any p Nil = False
any p (Cons n r) = myIf (p n) True (any p r)

pw :: (a -> a -> a) -> List a -> List a -> List a
pw f Nil _ = Nil
pw f _ Nil = Nil
pw f (Cons n r) (Cons m s) = Cons (f n m) (pw f r s)

fold :: (a -> a -> a) -> a -> List a -> a
fold f e Nil = e
fold f e (Cons n r) = f n (fold f e r)

unwrap :: Either a b -> b
unwrap (Left e) = error "Problem"
unwrap (Right x) = x

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile f (Cons n r) = myIf (f n) (Cons n (takeWhile f r)) Nil

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile f (Cons n r) = myIf (f n) (dropWhile f r) (Cons n r)