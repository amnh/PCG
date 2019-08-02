------------------------------------------------------------------------------
-- |
--
-- A benchmark for sumAndLength as discussed in the foldl library documentation.

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

module Main where

import Bio.Metadata.Dynamic
import Bio.Character.Encodable
import Bio.Character.Exportable
import Data.Bits
import Data.Foldable
import           Data.List.NonEmpty  (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Criterion.Main        as C (bench, bgroup, defaultMain, nf)


main :: IO ()
main = C.defaultMain
     [ C.bgroup "pcg-core"
       [ C.bench "pure-fp" $ C.nf (overlap sigma) medians
       ]
     ]

medians = foldr1 (<>) $ replicate 100 testList

testList :: NonEmpty DynamicCharacterElement
testList = fromExportableBuffer <$> NE.fromList
  [ ExportableCharacterSequence 1 512 [2^8 , 2^7 , 2^9  .|. 2^10, 2^32, 43267, 56445, 6345, 5436]
  , ExportableCharacterSequence 1 512 [2^1 , 2^6 , 2^7  .|. 2^19, 2^12, 4352564, 652256, 35464, 5436364]
  , ExportableCharacterSequence 1 512 [2^0 , 2^3 , 2^13 .|. 2^11, 2^2 , 2141, 569785, 432534, 8756]
  , ExportableCharacterSequence 1 512 [2^60, 2^5 , 2^47 .|. 2^31, 2^32, 344235, 465645, 56465465]
  , ExportableCharacterSequence 1 512 [2^51, 2^24, 2^49 .|. 2^13, 2^13, 45253, 3654635, 63463653, 13134]
  , ExportableCharacterSequence 1 512 [2^55, 2^13, 2^12 .|. 2^29, 2^61, 1243412345, 35454141, 514541,14]
  , ExportableCharacterSequence 1 512 [2^18, 2^23, 2^34 .|. 2^35, 2^58, 2431142145, 7864, 43, 6657576]
  , ExportableCharacterSequence 1 512 [2^45, 2^15, 2^25 .|. 2^37, 2^34, 932523, 7653423, 43543, 9879855]
  ]


sigma :: Word -> Word -> Word
sigma i j
  | i == j    = 0
  | otherwise = 1
