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

{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Exportable.Class where


import Control.Lens
import Foreign.C.Types 


-- |
-- Represents a sequence of fixed width characters packed into a bitwise form
-- consumable by lower level functions.
class Exportable c where

    toExportableBuffer     :: c -> ExportableCharacterSequence
    fromExportableBuffer   :: ExportableCharacterSequence -> c

    toExportableElements   :: c -> Maybe ExportableCharacterElements
    fromExportableElements :: ExportableCharacterElements -> c


-- |
-- A structure used for FFI calls.
--
-- 'bufferChunks' contains the bit-packed representation of the character sequence.
data ExportableCharacterSequence
   = ExportableCharacterSequence
   { exportedElementCountSequence :: Int
   , exportedElementWidthSequence :: Int
   , exportedBufferChunks :: [CULong]
   } deriving (Eq, Show)   


-- |
-- A structure used for FFI calls--
-- 'characterElements' contains the integral value for each character element.
data ExportableCharacterElements
   = ExportableCharacterElements
   { exportedElementCountElements :: Int
   , exportedElementWidthElements :: Int 
   , exportedCharacterElements :: [CUInt]
   } deriving (Eq, Show)   


-- |
-- A 'Lens' for the 'exportedElementCount' field
class HasExportedElementCount s a | s -> a where

    exportedElementCount :: Lens' s a
    {-# MINIMAL exportedElementCount #-}


-- | (✔)
instance HasExportedElementCount ExportableCharacterSequence Int where

    exportedElementCount = lens exportedElementCountSequence (\e x -> e { exportedElementCountSequence = x })


-- | (✔)
instance HasExportedElementCount ExportableCharacterElements Int where

    exportedElementCount = lens exportedElementCountElements (\e x -> e { exportedElementCountElements = x })


-- |
-- A 'Lens' for the 'exportedElementWidth' field
class HasExportedElementWidth s a | s -> a where

    exportedElementWidth :: Lens' s a
    {-# MINIMAL exportedElementWidth #-}


-- | (✔)
instance HasExportedElementWidth ExportableCharacterSequence Int where

    exportedElementWidth = lens exportedElementWidthSequence (\e x -> e { exportedElementWidthSequence = x })


-- | (✔)
instance HasExportedElementWidth ExportableCharacterElements Int where

    exportedElementWidth = lens exportedElementWidthElements (\e x -> e { exportedElementWidthElements = x })

