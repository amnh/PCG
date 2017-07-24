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
-- Currently is not functional.
--
----------------------------------------------------------------------------- 

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module PCG.Syntax.Parser where

import Data.List.NonEmpty     (NonEmpty)
import Data.Time.Clock        (DiffTime)
import PCG.Syntax.Combinators


-- |
-- This is old, probably don't use it, probably...
--
-- 'SyntacticCommand' is "Stringly-Typed" and therefore inherently unsafe.
-- We will later consume a list of SyntacticCommand as a Script type and
-- convert these into thier less dubious, well-type counterpart of type Command,
-- or report an error explaing why the SyntacticCommand is not valid.
data  SyntacticCommand
    = SyntacticCommand ListIdentifier (NonEmpty Argument)
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
    | ListIdArg      ListIdentifier
    | ListIdNamedArg ListIdentifier Argument
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
