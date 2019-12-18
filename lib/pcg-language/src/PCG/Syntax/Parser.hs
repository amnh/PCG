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

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE UnboxedSums        #-}

module PCG.Syntax.Parser where

import Data.CaseInsensitive   (FoldCase)
import Data.List.NonEmpty     (NonEmpty, some1)
import PCG.Command.Build
import PCG.Command.Echo
import PCG.Command.Load
import PCG.Command.Read
import PCG.Command.Report
import PCG.Command.Save
import PCG.Syntax.Combinators
import Text.Megaparsec


-- |
-- All the commands of the PCG scripting language.
data  Command
    = BUILD   {-# UNPACK #-} !BuildCommand
    | ECHO                   !EchoCommand
    | LOAD                   !LoadCommand
    | READ    {-# UNPACK #-} !ReadCommand
    | REPORT  {-# UNPACK #-} !ReportCommand
    | SAVE                   !SaveCommand
    deriving stock (Show)


-- |
-- A non-empty list of Commands to be sequentially evaluated.
newtype Computation = Computation (NonEmpty Command)
    deriving stock (Show)


-- |
-- Parse a series of PCG commands.
computationalStreamParser :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m Computation
computationalStreamParser = Computation <$> some1 commandStreamParser <* eof


-- |
-- Parse a single, well defined PCG command.
commandStreamParser :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m Command
commandStreamParser = whitespace *> choice
    [ BUILD  <$> parseCommand  buildCommandSpecification
    , ECHO   <$> parseCommand   echoCommandSpecification
    , READ   <$> parseCommand   readCommandSpecification
    , REPORT <$> parseCommand reportCommandSpecification
    , SAVE   <$> parseCommand   saveCommandSpecification
    , LOAD   <$> parseCommand   loadCommandSpecification
    ] <* whitespace
