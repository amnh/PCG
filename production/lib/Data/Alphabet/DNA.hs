-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Alphabet.DNA
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Data.Alphabet.DNA where

import Control.Arrow ((***))
import Data.Bimap
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

  
-- | Substitutions for converting to a DNA sequence based on IUPAC codes.
iupacNucleotideSubstitutions :: Map Char (NonEmpty String)
iupacNucleotideSubstitutions = M.fromList $
      (pure . pure *** fmap pure . NE.fromList) <$>
          [ ('A', "A")
          , ('C', "C")
          , ('G', "G")
          , ('T', "T")
          , ('R', "AG")
          , ('Y', "CT")
          , ('S', "CG")
          , ('W', "AT")
          , ('K', "GT")
          , ('M', "AC")
          , ('B', "CGT")
          , ('D', "AGT")
          , ('H', "ACT")
          , ('V', "ACG")
          , ('N', "ACGT")
          , ('-', "-")
          , ('.', "-")
          , ('?', "?")
          ] 
