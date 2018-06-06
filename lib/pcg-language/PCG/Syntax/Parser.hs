-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Syntax.Parser
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides the stream parser for interpreting a well-typed collection of
-- commands to be evaluated from an input source.
--
----------------------------------------------------------------------------- 

{-# LANGUAGE FlexibleContexts, TypeFamilies, UnboxedSums #-}

module PCG.Syntax.Parser where

import Data.CaseInsensitive   (FoldCase)
import Data.List.NonEmpty     (NonEmpty, some1)
import PCG.Command.Build
import PCG.Command.Read
import PCG.Command.Report
import PCG.Syntax.Combinators
import Text.Megaparsec


-- |
-- All the commands of the PCG scripting language.
data  Command
    = BUILD   {-# UNPACK #-} !BuildCommand
    | READ    {-# UNPACK #-} !ReadCommand
    | REPORT  {-# UNPACK #-} !ReportCommand
    deriving (Show)


-- |
-- A non-empty list of Commands to be sequentially evaluated.
newtype Computation = Computation (NonEmpty Command)
    deriving (Show)


-- |
-- Parse a series of PCG commands. 
computationalStreamParser :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m Computation
computationalStreamParser = Computation <$> some1 commandStreamParser <* eof


-- |
-- Parse a single, well defined PCG command.
commandStreamParser :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m Command
commandStreamParser = whitespace *> choice
    [ BUILD  <$> parseCommand  buildCommandSpecification
    , READ   <$> parseCommand   readCommandSpecification
    , REPORT <$> parseCommand reportCommandSpecification
    ] <* whitespace
