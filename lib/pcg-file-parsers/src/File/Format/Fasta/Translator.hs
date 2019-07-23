{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

module File.Format.Fasta.Translator where

import           Control.DeepSeq
import           Data.Key                   (lookup, (!))
import           Data.Map
import qualified Data.Map                   as M (fromList)
import qualified Data.Vector                as V (fromList)
import           File.Format.Fasta.Internal
import           File.Format.Fasta.Parser
import           GHC.Generics


data  FastaSequenceType
    = DNA
    | RNA
    | AminoAcid
    deriving stock (Data, Bounded, Eq, Enum, Generic, Read, Show, Typeable)
    deriving anyclass (NFData)
