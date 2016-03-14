{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Command.NStates where

import Data.Functor (($>))
import File.Format.TNT.Internal
import Text.Megaparsec
import Text.Megaparsec.Custom
import Text.Megaparsec.Prim     (MonadParsec)

-- | Parses NSTATES command
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

nstatesDna :: MonadParsec s m Char => m NStates
nstatesDna = identifier *> dnaStates
  where
    identifier  = symbol $ keyword "dna" 3
    gaps        = symbol $ string' "gaps"   $> True
    nogaps      = symbol $ string' "nogaps" $> False
    defaultGaps = pure True
    dnaStates   = DnaStates <$> (gaps <|> nogaps <|> defaultGaps)

nstatesNumeric :: MonadParsec s m Char => m NStates
nstatesNumeric = identifier *> stateCount
  where
    identifier = symbol $ keyword "numeric" 3
    stateCount = NumericStates <$> symbol nonNegInt

nstatesProtein :: MonadParsec s m Char => m NStates
nstatesProtein = identifier *> pure ProteinStates
  where
    identifier = symbol $ keyword "protein" 4

nstatesContinuous :: MonadParsec s m Char => m NStates
nstatesContinuous = identifier *> pure ContinuousStates
  where
    identifier  = symbol $ keyword "continuous" 4
