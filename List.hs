module List where

import Bool
import Either (Either (Left, Right), unwrap)
import Error
import Nat
import Prelude hiding (Bool, Either (Left, Right), and, any, concat, drop, dropWhile, elem, enumFromTo, filter, head, init, last, length, map, maximum, minimum, or, product, replicate, reverse, sum, tail, take, takeWhile, (++), False, True, (==), (<=), (>=), (<), (>), (+), (-), (*), (^), (/), (||), (/=), (||), (&&), all, max, min)

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

(++) :: List a -> List a -> List a
(++) Nil l = l
(++) (Cons x xs) l = Cons x (xs ++ l)

length :: List a -> Nat
length Nil = O
length (Cons n rest) = S (length rest)

take :: Nat -> List a -> List a
take (S n) (Cons m l) = Cons m (take n l)
take _ _ = Nil

drop :: Nat -> List a -> List a
drop _ Nil = Nil
drop O l = l
drop (S n) (Cons m l) = drop n l

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile f (Cons n r) = myIf (f n) (Cons n (takeWhile f r)) Nil

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile f (Cons n r) = myIf (f n) (dropWhile f r) (Cons n r)

fold :: (a -> a -> a) -> a -> List a -> a
fold f e Nil = e
fold f e (Cons n r) = f n (fold f e r)

pw :: (a -> a -> a) -> List a -> List a -> List a
pw f Nil _ = Nil
pw f _ Nil = Nil
pw f (Cons n r) (Cons m s) = Cons (f n m) (pw f r s)

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

head :: List a -> Either Error a
head Nil = Left NoHead
head (Cons n r) = Right n

tail :: List a -> Either Error (List a)
tail Nil = Left NoTail
tail (Cons n r) = Right r

init :: List a -> Either Error (List a)
init Nil = Left NoInit
init (Cons n Nil) = Right Nil
init (Cons n r) = Right (Cons n (unwrap (init r)))

last :: List a -> Either Error a
last Nil = Left NoLast
last (Cons n Nil) = Right n
last (Cons n r) = last r

replicate :: Nat -> a -> List a
replicate O a = Nil
replicate (S n) a = Cons a (replicate n a)

minimum :: List Nat -> Either Error Nat
minimum Nil = Left ListEmpty
minimum (Cons n Nil) = Right n
minimum (Cons m (Cons n r)) = minimum (Cons (min m n) r)

maximum :: List Nat -> Either Error Nat
maximum Nil = Left ListEmpty
maximum (Cons n Nil) = Right n
maximum (Cons m (Cons n r)) = maximum (Cons (max m n) r)

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

reverse :: List a -> List a
reverse Nil = Nil
reverse (Cons n rest) = reverse rest ++ Cons n Nil

enumFromTo :: Nat -> Nat -> List Nat
enumFromTo n m = myIf (n > m) Nil (Cons n (enumFromTo (S n) m))

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
addNat n = map (+ n)

multNat :: Nat -> List Nat -> List Nat
multNat n = map (* n)

expNat :: Nat -> List Nat -> List Nat
expNat n = map (^ n)

enumTo :: Nat -> List Nat
enumTo = enumFromTo O

-- elemIndices :: Nat -> List Nat -> List Nat

pwAdd :: List Nat -> List Nat -> List Nat
pwAdd = pw (+)

pwMult :: List Nat -> List Nat -> List Nat
pwMult = pw (*)

isSorted :: List Nat -> Bool
isSorted (Cons m (Cons n r)) = myIf (m <= n) (isSorted (Cons n r)) False
isSorted _ = True

filterEven :: List Nat -> List Nat
filterEven = filter ev

filterOdd :: List Nat -> List Nat
filterOdd = filter od

isPrefixOf :: List Nat -> List Nat -> Bool
isPrefixOf Nil Nil = True
isPrefixOf (Cons n r) Nil = False
isPrefixOf Nil (Cons n r) = True
isPrefixOf (Cons n r) (Cons m s) = myIf (n == m) (r `isPrefixOf` s) False

mix :: List Nat -> List Nat -> List Nat
mix Nil s = s
mix r Nil = r
mix (Cons n r) (Cons m s) = Cons n (Cons m (mix r s))

intersperse :: Nat -> List Nat -> List Nat
intersperse n Nil = Nil
intersperse n (Cons m Nil) = Cons m Nil
intersperse n (Cons m r) = Cons m (Cons n (intersperse n r))