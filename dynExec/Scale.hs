{-# LANGUAGE FlexibleInstances #-}

module Scale where

import Result

class Scale a where
  scale :: a -> [Value]

instance  Scale Int where
  scale x = [Value {valType = IntVal, value = x}]

instance {-# OVERLAPS #-} Scale [Int] where
  scale xs = [Value {valType = ListLen, value = length xs}]

instance {-# OVERLAPS #-} Scale a where
  scale _ = []