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
-- Provides the types for the Load command along with a semantic definition
-- to be consumed by the stream parser.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnboxedSums      #-}

module PCG.Command.Load
  ( LoadCommand (..)
  , loadCommandSpecification
  ) where

import PCG.Syntax.Combinators

data LoadCommand = LoadCommand !FilePath
  deriving Show

loadCommandSpecification :: CommandSpecification LoadCommand
loadCommandSpecification = command "Load" . argList $  LoadCommand <$> text
