module Util where

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) x f = f x
