-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Fasta
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for for parsing FASTA files into a naive sequence form.
--
----------------------------------------------------------------------------- 

module File.Format.Fasta
  ( FastaParseResult
  , FastaSequence(..)
  , FastaSequenceType(..)
  , TaxonSequenceMap
  , fastaStreamConverter
  , fastaStreamParser
  ) where


import File.Format.Fasta.Parser    (FastaParseResult,FastaSequence(..),fastaStreamParser) 
import File.Format.Fasta.Converter (FastaSequenceType(..),fastaStreamConverter)
import File.Format.Fasta.Internal  (TaxonSequenceMap)
