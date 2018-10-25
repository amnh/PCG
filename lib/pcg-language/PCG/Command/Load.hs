-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Load
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides the types for the \"LOAD\" command along with a semantic definition
-- to be consumed by the stream parser.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UnboxedSums        #-}

module PCG.Command.Load
  ( LoadCommand (..)
  , loadCommandSpecification
  ) where

import PCG.Command.Save
import PCG.Syntax.Combinators


-- |
-- The \"LOAD\" command specifies PCG to deserialize a save state from disk and
-- use that state as the new current state of the computation. The file path from
-- which to which the save state may be user specified. A default, hidden file
-- path will be used if no file path is specified by the user.
newtype LoadCommand = LoadCommand FilePath
  deriving stock Show


-- |
-- Defines the semantics of interpreting a valid \"LOAD\" command from the PCG
-- scripting language syntax.
loadCommandSpecification :: CommandSpecification LoadCommand
loadCommandSpecification = command "Load" . argList $  LoadCommand <$> (text `withDefault` defaultSaveFilePath)
