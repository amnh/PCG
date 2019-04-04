-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Save
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides the types for the \"SAVE\" command along with a semantic definition
-- to be consumed by the stream parser.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnboxedSums      #-}

module PCG.Command.Save
  ( SaveCommand (..)
  , SerialType(..)
  , serialType
  , saveCommandSpecification
  , defaultSaveFilePath
  ) where

import Control.Applicative.Free (Ap)
import Data.Functor             (($>))
import PCG.Syntax.Combinators


-- |
-- The \"SAVE\" command specifies PCG to serialize the current state of the
-- computation to disk. The file path to which the save state is serialized
-- may be user specified. A default, hidden file path exists if no file path is
-- specified by the user.
data SaveCommand = SaveCommand !FilePath !SerialType
  deriving Show


-- |
-- Type of serialisation formats
data SerialType
  = Compact
  | Binary
  deriving Show


-- |
-- Defines the semantics of interpreting a valid \"SAVE\" command from the PCG
-- scripting language syntax.
saveCommandSpecification :: CommandSpecification SaveCommand
saveCommandSpecification = command "save" . argList $ SaveCommand <$> (text `withDefault` defaultSaveFilePath) <*> (serialType `withDefault` defaultFormat)


-- |
-- Defines the serialization options.
serialType :: Ap SyntacticArgument SerialType
serialType = choiceFrom [saveCompact , saveBinary] `withDefault` defaultFormat
  where
    saveCompact  = value "compact" $> Compact
    saveBinary   = value "binary"  $> Binary


-- |
-- The default file path to serialize save states to when no path is specified
-- by the user.
defaultSaveFilePath :: FilePath
defaultSaveFilePath = ".pcg.save"


-- |
-- The default format to serialise to disk.
defaultFormat :: SerialType
defaultFormat = Compact
