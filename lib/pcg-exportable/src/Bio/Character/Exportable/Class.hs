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
-----------------------------------------------------------------------------

{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Bio.Character.Exportable.Class
  ( -- * Datatypes
    ExportableCharacterElements(..)
  , ExportableCharacterSequence(..)
    -- * Classes
  , Exportable(..)
  , HasExportedElementCount(..)
  , HasExportedElementWidth(..)
  ) where

import Control.Lens    (Lens', lens)
import Foreign.C.Types


-- |
-- A structure used for FFI calls.
--
-- 'exportedBufferChunks' contains the bit-packed representation of the character sequence.
data  ExportableCharacterSequence 
    = ExportableCharacterSequence
    { exportedElementCountSequence :: {-# UNPACK #-} !Word
    , exportedElementWidthSequence :: {-# UNPACK #-} !Word
    , exportedBufferChunks         :: ![CULong]
    }
    deriving stock (Eq, Show)


-- |
-- A structure used for FFI calls--
-- 'exportedCharacterElements' contains the integral value for each character element.
data  ExportableCharacterElements
    = ExportableCharacterElements
    { exportedElementCountElements :: {-# UNPACK #-} !Word
    , exportedElementWidthElements :: {-# UNPACK #-} !Word
    , exportedCharacterElements    :: ![CUInt]
    }
    deriving stock (Eq, Show)


-- |
-- Represents a sequence of fixed width characters packed into a bitwise form
-- consumable by lower level functions.
class Exportable c where

    toExportableBuffer     :: c -> ExportableCharacterSequence
    fromExportableBuffer   :: ExportableCharacterSequence -> c

    toExportableElements   :: c -> Maybe ExportableCharacterElements
    fromExportableElements :: ExportableCharacterElements -> c


-- |
-- A 'Control.Lens.Type.Lens' for the 'exportedElementCount' field
class HasExportedElementCount s a | s -> a where
    {-# MINIMAL exportedElementCount #-}

    exportedElementCount :: Lens' s a


-- |
-- A 'Control.Lens.Type.Lens' for the 'exportedElementWidth' field
class HasExportedElementWidth s a | s -> a where
    {-# MINIMAL exportedElementWidth #-}

    exportedElementWidth :: Lens' s a


instance HasExportedElementCount ExportableCharacterSequence Word where

    exportedElementCount = lens exportedElementCountSequence (\e x -> e { exportedElementCountSequence = x })


instance HasExportedElementCount ExportableCharacterElements Word where

    exportedElementCount = lens exportedElementCountElements (\e x -> e { exportedElementCountElements = x })


instance HasExportedElementWidth ExportableCharacterSequence Word where

    exportedElementWidth = lens exportedElementWidthSequence (\e x -> e { exportedElementWidthSequence = x })


instance HasExportedElementWidth ExportableCharacterElements Word where

    exportedElementWidth = lens exportedElementWidthElements (\e x -> e { exportedElementWidthElements = x })
