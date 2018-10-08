-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Encodable.Dynamic.Class
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

module Bio.Character.Encodable.Dynamic.Class where

import Bio.Character.Encodable.Internal
import Bio.Character.Encodable.Stream
import Data.Alphabet
import Data.MonoTraversable
import Data.Semigroup.Foldable
import Data.String                      (IsString)


-- |
-- Represents a character of variable length representing multiple encoded static characters.
--
-- > decodeMany alphabet . encodeMany alphabet == fmap toList . toList
--
class ( EncodableStream s
      , PossiblyMissingCharacter s
      ) => EncodableDynamicCharacter s where

    -- |
    -- Directly construct a dynamic character from the encoded elements.
    constructDynamic :: Foldable1 t => t (Element s) -> s

    encodeDynamic :: (Ord a, Foldable1 t, Foldable1 c, IsString a) => Alphabet a -> c (t a) -> s
