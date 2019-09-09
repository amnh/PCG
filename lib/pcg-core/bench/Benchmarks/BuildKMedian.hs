------------------------------------------------------------------------------
-- |
--
-- A benchmark for sumAndLength as discussed in the foldl library documentation.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}


module Main where

import           Benchmarks.Internal
import           Bio.Character.Encodable
import           Bio.Character.Exportable
import           Bio.Metadata.Dynamic
import           Data.Bits
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE


main :: IO ()
main =
  timeAndWeigh "overlap"
    [ ("select" , (overlap' sigma))
    , ("iterate", (overlap sigma))
    ]
    medians

medians :: NonEmpty DynamicCharacterElement
medians = foldr1 (<>) $ replicate 1000 testList

testList :: NonEmpty DynamicCharacterElement
testList = fromExportableBuffer <$> NE.fromList
  [ ExportableCharacterSequence 1 512 [2^8 , 2^7 , 2^9  .|. 2^10, 2^32, 2^32 - 1, 0, 2^57 - 1]
  , ExportableCharacterSequence 1 512 [2^1 , 2^6 , 2^7  .|. 2^19, 2^12, 2^32 - 1, 0, 2^57 - 1]
  , ExportableCharacterSequence 1 512 [2^0 , 2^3 , 2^13 .|. 2^11, 2^2 , 2^32 - 1, 0, 2^57 - 1]
  , ExportableCharacterSequence 1 512 [2^60, 2^5 , 2^47 .|. 2^31, 2^32, 2^32 - 1, 0, 2^57 - 1]
  , ExportableCharacterSequence 1 512 [2^51, 2^24, 2^49 .|. 2^13, 2^13, 2^32 - 1, 0, 2^57 - 1]
  , ExportableCharacterSequence 1 512 [2^55, 2^13, 2^12 .|. 2^29, 2^61, 2^32 - 1, 0, 2^57 - 1]
  , ExportableCharacterSequence 1 512 [2^18, 2^23, 2^34 .|. 2^35, 2^58, 2^32 - 1, 0, 2^57 - 1]
  , ExportableCharacterSequence 1 512 [2^45, 2^15, 2^25 .|. 2^37, 2^34, 2^32 - 1, 0, 2^57 - 1]
  ]


sigma :: Word -> Word -> Word
sigma i j
  | i == j    = 0
  | otherwise = 1
