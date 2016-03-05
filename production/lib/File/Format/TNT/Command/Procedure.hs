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

-- | A directive to interpret a file. We throw this info away later.
-- Interpreting a file kinda sucks, this ins't Lisp.
procCommandFile :: MonadParsec s m Char => m FilePath
procCommandFile = anythingTill (whitespace *> char ';') <* trim (char ';')

-- | A directive to read in a FASTA file as aligned, non-addative data.
-- Not sure if we should ignore this or acturally process additional files.
procFastaFile :: MonadParsec s m Char => m FilePath
procFastaFile = symbol (char '&') *> procCommandFile

-- | A close file directive. Closes all open files. Found at the end of all
-- properly formated TNT input files.
procCloseFile :: MonadParsec s m Char => m ()
procCloseFile = symbol (char '/') *> symbol (char ';') *> pure ()
