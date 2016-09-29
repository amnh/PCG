-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Static.Class
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

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Bio.Character.Static.Class where

import Bio.Character.Stream
import Data.Alphabet
import Data.List.NonEmpty
import Data.MonoTraversable
import Data.Semigroup

class EncodableStreamElement c => EncodableStaticCharacter c where

  encodeStatic :: Eq a => Alphabet a -> NonEmpty a -> c
  encodeStatic = encodeElement


class ( EncodableStaticCharacter (Element s)
      , EncodableStream s
      , Semigroup s
      ) => EncodableStaticCharacterStream s where

    -- |
    -- Directly construct a static character stream from the encoded elements.
    constructStaticStream :: NonEmpty (Element s) -> s


