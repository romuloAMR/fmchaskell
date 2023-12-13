module Either where
import Prelude hiding(Either, Left, Right)
data Either a b = Left a | Right b
  deriving ( Eq , Show )

unwrap :: Either a b -> b
unwrap (Left e) = error "Problem"
unwrap (Right x) = x