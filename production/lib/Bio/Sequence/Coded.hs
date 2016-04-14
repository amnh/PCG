-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequences.Coded
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data structures and instances for coded sequences
-- TODO: Explain what the heck a coded sequence is, and what it's used for.
--
-----------------------------------------------------------------------------

module Bio.Sequence.Coded (CodedSequence(..), EncodedSeq, EncodedSequences, CodedChar(..), encodeAll, decodeMany, module Bio.Sequence.Coded.Random) where

import Bio.Sequence.Coded.Internal
import Bio.Sequence.Coded.Class
import Bio.Sequence.Coded.Random
import Bio.Sequence.Character.Coded

