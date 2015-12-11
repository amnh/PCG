module Bio.Sequence.Parsed where

import Bio.Sequence.Coded
import Data.Vector
import qualified Data.Map.Lazy as M

type ParsedSequence = M.Map String (Vector (Vector [String]))
