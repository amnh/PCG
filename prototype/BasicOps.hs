module BasicOps where

import Prelude hiding (head)
import Data.Vector hiding ((!), head)

(!) :: Vector a -> Int -> a
(!) = unsafeIndex

head :: Vector a -> a
head = unsafeHead