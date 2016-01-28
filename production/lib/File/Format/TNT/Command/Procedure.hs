{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Command.Procedure where

{-- TODO:
  - Robust tests
  - Good documentation
  -}

import           Data.Bifunctor           (second)
import           Data.Char                (isSpace)
import           Data.DList               (DList,append)
import qualified Data.DList         as DL (toList,fromList)
import           Data.IntSet              (IntSet, singleton)
import qualified Data.IntSet        as IS (fromList)
import           Data.List                (intersperse)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (filter,fromList,length)
import           Data.Map.Strict          (Map,insertWith)
import qualified Data.Map.Strict    as M  (toList)
import           Data.Maybe               (catMaybes)
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Lexer    (integer,number,signed)
import           Text.Megaparsec.Prim     (MonadParsec)

-- | Parses an PROCEDURE command that consisits of exacty
-- one of the following:
--
--  * Close file directive
--
--  * Fasta file to read-in
--
--  * Command file to be interpreted
procCommand :: MonadParsec s m Char => m ()
procCommand =  procHeader *> procBody
  where
    procBody = (try procFastaFile   *> pure ())
           <|> (try procCommandFile *> pure ())
           <|>      procCloseFile

-- | Consumes the superflous heading for a PROCEDURE command.
procHeader :: MonadParsec s m Char => m ()
procHeader = string' "proc" *> optional (string' "edure") *> pure ()

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
