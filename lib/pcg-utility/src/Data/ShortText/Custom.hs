-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ShortText.Custom
-- Copyright   :  (c) 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility Functions for ShortText.
--
-----------------------------------------------------------------------------


module Data.ShortText.Custom
  ( intToShortText
  , makeIllegalShortText
  , convertToBytes
  ) where

import Data.Text.Short (ShortText, fromString)
import Data.ByteString.Short hiding (unpack)
import Data.Text.Short.Unsafe (fromShortByteStringUnsafe)
import Data.Word
import Data.ByteString.Builder
import Data.ByteString.Lazy (unpack)



-- |
-- Converts a signed integer to ShortText.
intToShortText :: Int -> ShortText
intToShortText = fromString . show

-- |
-- The purpose of this function is to create non-UTF-8 valid 'ShortText' which is unique for each input
-- to be used to give temporary names which are guaranteed to be distinct from those names already in the
-- graph.
makeIllegalShortText :: Word64 -> ShortText
makeIllegalShortText = fromShortByteStringUnsafe . makeNonUTFByteString

-- |
-- Creates a ByteString which is invalid UTF8
makeNonUTFByteString :: Word64 -> ShortByteString
                        -- 192 is not allowed as a starting code point in UTF-8
makeNonUTFByteString = pack . (\bytes -> 192 : bytes) . convertToBytes


convertToBytes :: Word64 -> [Word8]
convertToBytes = unpack . toLazyByteString . word64BE
