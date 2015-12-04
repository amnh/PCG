module Data.Sequence.Packed where

import Data.Sequence.Coded

type PackedSeq = Maybe (Vector (PackedChar b))
type PackedChar b = Vector b