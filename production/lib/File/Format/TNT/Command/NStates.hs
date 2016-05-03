----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TNT.Command.NStates
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Parser for the NSTATES command specifying how to interpret the character
-- states for various character types.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Command.NStates where

import Data.Functor (($>))
import File.Format.TNT.Internal
import Text.Megaparsec
import Text.Megaparsec.Prim     (MonadParsec)

-- | Parses NSTATES command.
nstatesCommand :: MonadParsec s m Char => m NStates
nstatesCommand =  nstatesHeader *> nstatesBody <* symbol (char ';')
  where
    nstatesBody = choice
                [ nstatesDna
                , nstatesNumeric
                , nstatesProtein
                , nstatesContinuous
                ]

-- | Consumes the superflous heading for a NSTATES command.
nstatesHeader :: MonadParsec s m Char => m ()
nstatesHeader = symbol $ keyword "nstates" 2

-- | Parses the specifaction for interpreting dna characters.
nstatesDna :: MonadParsec s m Char => m NStates
nstatesDna = identifier *> dnaStates
  where
    identifier  = symbol $ keyword "dna" 3
    gaps        = symbol $ string' "gaps"   $> True
    nogaps      = symbol $ string' "nogaps" $> False
    defaultGaps = pure True
    dnaStates   = DnaStates <$> (gaps <|> nogaps <|> defaultGaps)

-- | Parses the specification for interpreting numeric/discrete characters.
nstatesNumeric :: MonadParsec s m Char => m NStates
nstatesNumeric = identifier *> stateCount
  where
    identifier = symbol $ keyword "numeric" 3
    stateCount = NumericStates <$> symbol nonNegInt

-- | Parses the specifaction for interpreting protein characters.
nstatesProtein :: MonadParsec s m Char => m NStates
nstatesProtein = identifier $> ProteinStates
  where
    identifier = symbol $ keyword "protein" 4

-- | Parses the specifaction for interpreting continuous characters.
nstatesContinuous :: MonadParsec s m Char => m NStates
nstatesContinuous = identifier $> ContinuousStates
  where
    identifier  = symbol $ keyword "continuous" 4
