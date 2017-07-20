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


data  Syntax
    = Syntax (NonEmpty SyntacticCommand)
    deriving (Show)


data  Argument
    = PrimativeArg   Primative
    | ListIdArg      ListIdentifier
    | ListIdNamedArg ListIdentifier Argument
    | CommandArg     SyntacticCommand
    | ArgumentList  (NonEmpty Argument)
    deriving (Show)


data  Primative
    = WholeNum  Int
    | RealNum   Double
    | BitValue  Bool
    | TextValue String
    | TimeSpan  DiffTime
    deriving (Show)
