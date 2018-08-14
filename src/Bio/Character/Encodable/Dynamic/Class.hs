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
--import Data.List.NonEmpty
import Data.MonoTraversable
import Data.String                      (IsString)

-- {-# DEPRECATED decodeDynamic "Don't use decodeDynamic, use decodeStream instead!" #-}
{-# DEPRECATED encodeDynamic "Don't use encodeDynamic, use encodeStream instead!" #-}
-- {-# DEPRECATED indexChar     "Don't use indexChar, use indexStream instead!"      #-}
-- {-# DEPRECATED lookupChar    "Don't use lookupChar, use lookupStream instead!"    #-}

-- TODO: Add more laws here
{- | Represents a character of variable length representing multiple encoded static characters.

 decodeMany alphabet . encodeMany alphabet == fmap toList . toList

-}
class ( EncodableStream s
      , PossiblyMissingCharacter s
      ) => EncodableDynamicCharacter s where

    -- |
    -- Directly construct a dynamic character from the encoded elements.
    constructDynamic :: Foldable t => t (Element s) -> s
-- TODO: Make sure it's non-empty
--    constructDynamic :: NonEmpty (Element s) -> s

--    decodeDynamic :: (Ord a, IsString a) => Alphabet a -> s -> [[a]]
--    decodeDynamic alphabet = toList . (fmap toList) . decodeStream alphabet

    encodeDynamic :: (Ord a, Foldable t, Foldable c, IsString a) => Alphabet a -> c (t a) -> s

--    indexChar  :: s -> Int -> Element s
--    indexChar  = indexStream

--    lookupChar :: s -> Int -> Maybe (Element s)
--    lookupChar = lookupChar

