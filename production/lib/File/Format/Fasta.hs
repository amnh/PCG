module File.Format.Fasta
  ( FastaSequence(..)
  , FastaSequenceType(..)
  , TaxonSequenceMap
  , fastaStreamConverter
  , fastaStreamParser
  ) where


import File.Format.Fasta.Parser    (FastaSequence(..),fastaStreamParser) 
import File.Format.Fasta.Converter (FastaSequenceType(..),fastaStreamConverter)
import File.Format.Fasta.Internal  (TaxonSequenceMap)
