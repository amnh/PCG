module Bio.Sequence.Packed where

import Bio.Sequence.Coded

type PackedSeq = Maybe (Vector (PackedChar b))
type PackedChar b = Vector b