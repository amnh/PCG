-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Version
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides the types for the \"Version\" command along with a semantic definition
-- to be consumed by the stream parser.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UnboxedSums        #-}

module PCG.Command.Version
  ( VersionCommand (..)
  , versionCommandSpecification
  ) where

import Data.Functor           (($>))
import PCG.Syntax.Combinators


newtype VersionCommand =
        VersionCommand
        { fullVersion :: Bool
        } deriving stock (Show)


versionCommandSpecification :: CommandSpecification VersionCommand
versionCommandSpecification = command "version" . argList $ version
  where
    version = VersionCommand <$> options `withDefault` False
    options = choiceFrom [ value "full" $> True, value "short" $> False]
