-----------------------------------------------------------------------------
-- |
-- Module      :  FFI.Character.Exportable
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

module FFI.Character.Exportable
  ( Exportable(..)
  , HasExportedElementCount(..)
  , HasExportedElementWidth(..)
  , ExportableCharacterElements(ExportableCharacterElements, exportedCharacterElements)
  , ExportableCharacterSequence(ExportableCharacterSequence, exportedBufferChunks)
  ) where

import FFI.Character.Exportable.Class
