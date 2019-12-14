-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Echo
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides the types for the \"ECHO\" command along with a semantic definition
-- to be consumed by the stream parser.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DerivingStrategies #-}

module PCG.Command.Echo
  ( EchoCommand (..)
  , echoCommandSpecification
  ) where

import Data.Text.Short
import PCG.Syntax.Combinators


-- |
-- The \"ECHO\" command specifies PCG to serialize the current state of the
-- computation to disk. The file path to which the echo state is serialized
-- may be user specified. A default, hidden file path exists if no file path is
-- specified by the user.
newtype EchoCommand = EchoCommand ShortText
  deriving stock (Show)


-- |
-- Defines the semantics of interpreting a valid \"ECHO\" command from the PCG
-- scripting language syntax.
echoCommandSpecification :: CommandSpecification EchoCommand
echoCommandSpecification = command "echo" . argList $ EchoCommand <$> text
