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

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}

module File.Format.TNT.Command.NStates where


import Data.CaseInsensitive
import Data.Functor (($>))
import Data.Proxy
import File.Format.TNT.Internal
import Text.Megaparsec
import Text.Megaparsec.Char


-- |
-- Parses NSTATES command.
nstatesCommand :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m NStates
nstatesCommand =  nstatesHeader *> nstatesBody <* symbol (char ';')
  where
    nstatesBody = choice
        [ nstatesDna
        , nstatesNumeric
        , nstatesProtein
        , nstatesContinuous
        ]


-- |
-- Consumes the superflous heading for a NSTATES command.
nstatesHeader :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m ()
nstatesHeader = symbol $ keyword "nstates" 2


-- |
-- Parses the specifaction for interpreting dna characters.
nstatesDna :: forall e s m. (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m NStates
nstatesDna = identifier *> dnaStates
  where
    identifier  = symbol $ keyword "dna" 3
    gaps        = symbol $ string' (tokensToChunk proxy "gaps"  ) $> True
    nogaps      = symbol $ string' (tokensToChunk proxy "nogaps") $> False
    defaultGaps = pure True
    dnaStates   = DnaStates <$> (gaps <|> nogaps <|> defaultGaps)
    proxy       = Proxy :: Proxy s


-- |
-- Parses the specification for interpreting numeric/discrete characters.
nstatesNumeric :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m NStates
nstatesNumeric = identifier *> stateCount
  where
    identifier = symbol $ keyword "numeric" 3
    stateCount = NumericStates <$> symbol nonNegInt


-- |
-- Parses the specifaction for interpreting protein characters.
nstatesProtein :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m NStates
nstatesProtein = identifier $> ProteinStates
  where
    identifier = symbol $ keyword "protein" 4


-- |
-- Parses the specifaction for interpreting continuous characters.
nstatesContinuous :: (FoldCase (Tokens s), MonadParsec e s m, Token s ~ Char) => m NStates
nstatesContinuous = identifier $> ContinuousStates
  where
    identifier  = symbol $ keyword "continuous" 4
