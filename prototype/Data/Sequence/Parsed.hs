module Data.Sequence.Parsed where

import Data.Sequence.Encoded
import Data.Vector

type Parsed = Vector [String]

class ParsedSequence s where 
    encode :: s -> EncodedSeq