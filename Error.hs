module Error where

data Error = DivZero | ListEmpty | NoHead | NoTail | NoInit | NoLast 
  deriving ( Eq , Show )