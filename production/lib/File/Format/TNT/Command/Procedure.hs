----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TNT.Command.Procedure
-- Copyright   :  (c) 2015-2015 Ward Wheeler
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
module File.Format.TNT.Command.Procedure where

import Data.Functor (($>))
import File.Format.TNT.Internal
import Text.Megaparsec
import Text.Megaparsec.Custom
import Text.Megaparsec.Prim     (MonadParsec)

-- | Parses an PROCEDURE command that consisits of exacty
-- one of the following:
--
--  * Close file directive
--
--  * Fasta file to read-in
--
--  * Command file to be interpreted
procedureCommand :: MonadParsec s m Char => m ()
procedureCommand =  procHeader *> procBody
  where
    procBody = choice
             [ try procFastaFile   $> ()
             , try procCommandFile $> ()
             , procCloseFile
             ]

-- | Consumes the superflous heading for a PROCEDURE command.
procHeader :: MonadParsec s m Char => m ()
procHeader = symbol $ keyword "procedure" 4

-- | A directive to load and interpret the specified TNT file.
--   This interpretation is beyond the scope of this software.
--   We ignore PROCEDURE commands of this form .
procCommandFile :: MonadParsec s m Char => m FilePath
procCommandFile = anythingTill (whitespace *> char ';') <* trim (char ';')

-- | A directive to read in a FASTA file as aligned, non-addative data.
--   This interpretation is beyond the scope of this software.
--   We ignore PROCEDURE commands of this form. 
procFastaFile :: MonadParsec s m Char => m FilePath
procFastaFile = symbol (char '&') *> procCommandFile

-- | A close file directive. Closes all open files. Found at the end of all
--   properly formated TNT input files.
--   This software does not open files for interpretation from a TNT file,
--   so this command will have no effect and be ignored.
procCloseFile :: MonadParsec s m Char => m ()
procCloseFile = symbol (char '/') *> symbol (char ';') $> ()
