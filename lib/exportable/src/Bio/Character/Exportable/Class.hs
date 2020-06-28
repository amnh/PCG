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
{-# LANGUAGE TypeFamilies           #-}

module Bio.Character.Exportable.Class
  ( -- * Datatypes
    ExportableCharacterBuffer(..)
  , ExportableCharacterElements(..)
  , ReImportableCharacterElements(..)
  , Subcomponent
    -- * Classes
  , ExportableBuffer(..)
  , ExportableElements(..)
  , HasExportedElementCount(..)
  , HasExportedElementWidth(..)
  ) where

import Control.Lens    (Lens', lens)
import Data.MonoTraversable
import Foreign.C.Types


type family Subcomponent median


-- |
-- A structure used for FFI calls.
--
-- 'exportedBufferChunks' contains the bit-packed representation of the character sequence.
data  ExportableCharacterBuffer
    = ExportableCharacterBuffer
    { exportedElementCountBuffer :: {-# UNPACK #-} !Word
    , exportedElementWidthBuffer :: {-# UNPACK #-} !Word
    , exportedBufferChunks       :: ![CULong]
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
-- A structure used for FFI calls--
-- 'exportedCharacterElements' contains the integral value for each character element.
data  ReImportableCharacterElements
    = ReImportableCharacterElements
    { reimportableElementCountElements :: {-# UNPACK #-} !Word
    , reimportableElementWidthElements :: {-# UNPACK #-} !Word
    , reimportableCharacterElements    :: ![(CUInt, CUInt, CUInt)]
    }
    deriving stock (Eq, Show)


-- |
-- Represents a sequence of fixed width characters packed into a bitwise form
-- consumable by lower level functions.
class ExportableBuffer c where

    toExportableBuffer     :: c -> ExportableCharacterBuffer

    fromExportableBuffer   :: ExportableCharacterBuffer -> c


-- |
-- Represents a sequence of fixed width characters packed into a bitwise form
-- consumable by lower level functions.
class ExportableElements c where

    toExportableElements   :: (Subcomponent (Element c) -> Subcomponent (Element c) -> Subcomponent (Element c)) -> c -> Maybe ExportableCharacterElements

    fromExportableElements :: ReImportableCharacterElements -> c


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


instance HasExportedElementCount ExportableCharacterBuffer Word where

    exportedElementCount = lens exportedElementCountBuffer (\e x -> e { exportedElementCountBuffer = x })


instance HasExportedElementCount ExportableCharacterElements Word where

    exportedElementCount = lens exportedElementCountElements (\e x -> e { exportedElementCountElements = x })


instance HasExportedElementCount ReImportableCharacterElements Word where

    exportedElementCount = lens reimportableElementCountElements (\e x -> e { reimportableElementCountElements = x })


instance HasExportedElementWidth ExportableCharacterBuffer Word where

    exportedElementWidth = lens exportedElementWidthBuffer (\e x -> e { exportedElementWidthBuffer = x })


instance HasExportedElementWidth ExportableCharacterElements Word where

    exportedElementWidth = lens exportedElementWidthElements (\e x -> e { exportedElementWidthElements = x })


instance HasExportedElementWidth ReImportableCharacterElements Word where

    exportedElementWidth = lens reimportableElementWidthElements (\e x -> e { reimportableElementWidthElements = x })
