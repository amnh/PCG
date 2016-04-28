-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Packed.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for needed operations of packed sequences
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module Bio.Sequence.Packed.Class where

import Bio.Sequence.Parsed
import Bio.Sequence.Coded.Class

<<<<<<< HEAD

class EncodableDynamicCharacter s => PackedSequence s where
    packOverAlphabet :: ParsedSeq -> AmbiguityGroup -> s
=======
-- | Defines type for which a sequence can be converted into compact bit-valued
--   representation.
class CodedSequence s => PackedSequence s where
    packOverAlphabet :: ParsedSeq -> AmbiguityGroup -> s
>>>>>>> master
