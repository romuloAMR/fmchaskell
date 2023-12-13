module Bool where

import Prelude hiding (Bool, False, True, not, (&&), (||))

data Bool = True | False
  deriving (Eq, Show)

(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _ _ = False

(||) :: Bool -> Bool -> Bool
(||) True _ = True
(||) _ True = True
(||) _ _ = False

not :: Bool -> Bool
not True = False
not False = True

myIf :: Bool -> a -> a -> a
myIf True x y = x
myIf False x y = y