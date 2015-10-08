module File.Format.Fasta
  ( FastaSequence(..)
  , FastaSequenceType(..)
  , fastaStreamConverter
  , fastaStreamParser
  ) where

 import File.Format.Fasta.Parser    (FastaSequence(..),fastaStreamParser) 
 import File.Format.Fasta.Converter (FastaSequenceType(..),fastaStreamConverter)
