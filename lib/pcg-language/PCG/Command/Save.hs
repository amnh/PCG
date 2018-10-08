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

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UnboxedSums        #-}

module PCG.Command.Save
  ( SaveCommand (..)
  , saveCommandSpecification
  , defaultSaveFilePath
  ) where

import PCG.Syntax.Combinators


-- |
-- The \"SAVE\" command specifies PCG to serialize the current state of the
-- computation to disk. The file path to which the save state is serialized
-- may be user specified. A default, hidden file path exists if no file path is
-- specified by the user.
newtype SaveCommand = SaveCommand FilePath
  deriving stock Show


-- |
-- Defines the semantics of interpreting a valid \"SAVE\" command from the PCG
-- scripting language syntax.
saveCommandSpecification :: CommandSpecification SaveCommand
saveCommandSpecification = command "save" . argList $ SaveCommand <$> (text `withDefault` defaultSaveFilePath)


-- |
-- The default file path to serialize save states to when no path is specified
-- by the user.
defaultSaveFilePath :: FilePath
defaultSaveFilePath = ".pcg.save"
