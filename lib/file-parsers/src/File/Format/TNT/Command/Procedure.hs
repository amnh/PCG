----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TNT.Command.Procedure
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Parser for the procedure command. The parse results of the procedure parser
-- are usually ignored by the calling combinators.
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module File.Format.TNT.Command.Procedure
  ( procedureCommand
  , procCommandFile
  , procCloseFile
  , procHeader
  ) where

import Data.CaseInsensitive     (FoldCase)
import Data.Functor             (($>))
import File.Format.TNT.Internal
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Custom


-- |
-- Parses an PROCEDURE command that consisits of exactly one of the following:
--
--  * Close file directive
--
--  * Fasta file to read-in
--
--  * Command file to be interpreted
procedureCommand :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m ()
procedureCommand =  procHeader *> procBody
  where
    procBody = choice
        [ try procFastaFile   $> ()
        , try procCommandFile $> ()
        , procCloseFile
        ]


-- |
-- Consumes the superfluous heading for a PROCEDURE command.
procHeader :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m ()
procHeader = symbol $ keyword "procedure" 4


-- |
-- A directive to load and interpret the specified TNT file.
-- This interpretation is beyond the scope of this software.
-- We ignore PROCEDURE commands of this form .
procCommandFile :: (MonadParsec e s m, Token s ~ Char) => m FilePath
procCommandFile = anythingTill (whitespace *> char ';') <* trim (char ';')


-- |
-- A directive to read in a FASTA file as aligned, non-addative data.
-- This interpretation is beyond the scope of this software.
-- We ignore PROCEDURE commands of this form.
procFastaFile :: (MonadParsec e s m, Token s ~ Char) => m FilePath
procFastaFile = symbol (char '&') *> procCommandFile


-- |
-- A close file directive. Closes all open files. Found at the end of all
-- properly formatted TNT input files.
-- This software does not open files for interpretation from a TNT file,
-- so this command will have no effect and be ignored.
procCloseFile :: (MonadParsec e s m, Token s ~ Char) => m ()
procCloseFile = symbol (char '/') *> symbol (char ';') $> ()
