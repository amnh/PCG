-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Exportable.Class
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

module Bio.Character.Exportable.Class where

import Foreign.C.Types 

{- | Represents a sequence of fixed width characters packed into a bitwise form
 -   consumable by lower level functions.
 -}
class Exportable c where
    toExportable   :: c -> ExportableCharacterSequence
    fromExportable :: ExportableCharacterSequence -> c

-- | A structure used for FFI calls (and maybe GPU stuff)
data ExportableCharacterSequence
   = ExportableCharacterSequence
   { characterCount :: Int
   , characterWidth :: Int
   , bufferChunks   :: [CULong]
   } deriving (Eq, Show)   
