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

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module PCG.Syntax.Parser where

import Data.CaseInsensitive   (FoldCase)
import Data.List.NonEmpty     (NonEmpty, some1)
import Data.Time.Clock        (DiffTime)
import PCG.Command.Read
import PCG.Command.Report
import PCG.Syntax.Combinators
import Text.Megaparsec


-- |
-- All the commands of the PCG scripting language.
data  Command
    = READ   ReadCommand
    | REPORT ReportCommand
    deriving (Show)


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
    [ READ   <$> parseCommand   readCommandSpecification
    , REPORT <$> parseCommand reportCommandSpecification
    ] <* whitespace


{-
-- |
-- This is old, probably don't use it, probably...
--
-- 'SyntacticCommand' is "Stringly-Typed" and therefore inherently unsafe.
-- We will later consume a list of SyntacticCommand as a Script type and
-- convert these into thier less dubious, well-type counterpart of type Command,
-- or report an error explaing why the SyntacticCommand is not valid.
data  SyntacticCommand
    = SyntacticCommand ArgumentIdentifier (NonEmpty Argument)
    deriving (Show)


-- |
-- Probably will eventually retro-fit this.
data  Syntax
    = Syntax (NonEmpty SyntacticCommand)
    deriving (Show)


-- |
-- Again old, probable don't use.
data  Argument
    = PrimativeArg   Primative
    | ListIdArg      ArgumentIdentifier
    | ListIdNamedArg ArgumentIdentifier Argument
    | CommandArg     SyntacticCommand
    | ArgumentList  (NonEmpty Argument)
    deriving (Show)


-- |
-- Again old, probable don't use.
data  Primative
    = WholeNum  Int
    | RealNum   Double
    | BitValue  Bool
    | TextValue String
    | TimeSpan  DiffTime
    deriving (Show)
-}
