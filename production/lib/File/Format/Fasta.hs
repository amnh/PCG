module File.Format.Fasta
  ( FastaSequence(..)
  , FastaSequenceType(..)
  , fastaStreamTranslator
  , fastaStreamParser
  ) where

 import File.Format.Fasta.Parser     (FastaSequence(..),fastaStreamParser) 
 import File.Format.Fasta.Translator (FastaSequenceType(..),fastaStreamTranslator)
