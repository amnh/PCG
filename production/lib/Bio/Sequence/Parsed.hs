module Bio.Sequence.Parsed where

import Bio.Sequence.Encoded
import Data.Vector

type Parsed = Vector [String]

class ParsedSequence s where 
    encode :: s -> EncodedSeq