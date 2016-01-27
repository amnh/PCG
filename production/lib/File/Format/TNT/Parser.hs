{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Parser where

{-- TODO:
  - Robust tests
  - Good documentation
  - Deinterleave function with DList construction
  -}

import           Data.List.NonEmpty
import           File.Format.TNT.Command.CCode
import           File.Format.TNT.Command.Procedure
import           File.Format.TNT.Command.XRead
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Lexer    (integer,number,signed)
import           Text.Megaparsec.Prim     (MonadParsec)

-- TODO: make the types better
henningStreamParser :: MonadParsec s m Char => m (NonEmpty TaxonInfo, [CharacterChange])
henningStreamParser = do
  leading   <- many $ symbol ccodeCommand
  taxaStuff <- symbol xreadCommand
  trailing  <- many $ symbol ccodeCommand
  pure (taxaStuff, leading ++ trailing)
