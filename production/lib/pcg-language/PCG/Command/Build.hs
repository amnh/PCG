-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Build
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides the types fot the Report command allong with a semantic definition
-- to be consumed by the stream parser.
--
-----------------------------------------------------------------------------

module PCG.Command.Build
  ( BuildCommand(..)
  , buildCommandSpecification
  ) where


import PCG.Syntax.Combinators


-- |
-- The REPORT command specifying what information should be output and where the
-- output should be directed.
data  BuildCommand
    = BuildCommand
    deriving (Show)

-- |
-- Defines the semantics of interpreting a valid \"Report\" command from the PCG
-- scripting language syntax.
buildCommandSpecification :: CommandSpecification BuildCommand
buildCommandSpecification = command "build" . argList $ pure BuildCommand
