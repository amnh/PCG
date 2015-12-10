module Bio.Sequence.Packed where

import Data.Vector

type PackedSeq b = Maybe (Vector (PackedChar b))
type PackedChar b = Vector b
