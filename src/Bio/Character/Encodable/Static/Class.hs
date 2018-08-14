-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Static.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for needed operations of coded sequences and characters
--
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bio.Character.Encodable.Static.Class where

import Bio.Character.Encodable.Internal
import Bio.Character.Encodable.Stream
import Data.Alphabet
import Data.Bits
import Data.List.NonEmpty               hiding (xor)
import Data.MonoTraversable
import Data.String                      (IsString)



-- |
-- Represents a single static character encoded in binary and coercable to
-- `Int` values.
--
-- Binary encoding must allow for the nth symbol in the 'Alphabet' to be tested
-- for membership in the encoded ambiguity group by the following computation:
--
-- >>> staticCharacter `testBit` n
class ( EncodableStreamElement c
      , PossiblyMissingCharacter c
      ) => EncodableStaticCharacter c where

    encodeStatic :: (Eq a, IsString a) => Alphabet a -> AmbiguityGroup a -> c
    encodeStatic = encodeElement

    emptyStatic :: c -> c
    emptyStatic x = x `xor` x


-- |
-- Represents a fixed number of static charcters encoded over 'Alphabet's of the
-- same size and with the same cost structure.
class ( EncodableStaticCharacter (Element s)
      , EncodableStream s
      , Semigroup s
      ) => EncodableStaticCharacterStream s where

    -- |
    -- Directly construct a static character stream from the encoded elements.
    constructStaticStream :: NonEmpty (Element s) -> s
