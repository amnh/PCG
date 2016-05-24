----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TNT.Command.CCode
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- CCode command parser.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module File.Format.TNT.Command.CCode where

import File.Format.TNT.Internal
import Text.Megaparsec
import Text.Megaparsec.Custom   (nonEmpty)
import Text.Megaparsec.Prim     (MonadParsec)

-- | Parses a CCODE command that consists of:
--
--  * One ore more specifications of the character state change
--
--  * One or more character indicies or index ranges of affected characters
ccodeCommand :: (MonadParsec e s m, Token s ~ Char) => m CCode
ccodeCommand = ccodeHeader *> nonEmpty ccodeAugment <* symbol (char ';')

-- | The header of a CCODE command.
ccodeHeader :: (MonadParsec e s m, Token s ~ Char) => m ()
ccodeHeader = symbol $ keyword "ccode" 2

-- | A 'CharacterMetadata' mutation specified by the CCODE command.
--   Mutations are specified for a nonempty set of characrter index ranges
--   and also a nonempty set of metadata values.
--   Validates that mutulally exclusive metadata options are not specified.
ccodeAugment :: (MonadParsec e s m, Token s ~ Char) => m CCodeAugment
ccodeAugment = CCodeAugment
           <$> (validateStates =<< nonEmpty ccodeCharacterState)
           <*> nonEmpty characterIndicies
           <*  allowIncorrectSuffixes
  where
    makeError str = fail $ concat ["Specified both '",str,"' and 'non-",str,"' states for character set in CCODE command"]
    validateStates xs
      | Additive `elem` xs && NonAdditive `elem` xs = makeError "additive"
      | Active   `elem` xs && NonActive   `elem` xs = makeError "active"
      | Sankoff  `elem` xs && NonSankoff  `elem` xs = makeError "sankoff"
      | Additive `elem` xs && Sankoff     `elem` xs = fail $ concat ["Specified both 'additive' and 'sankoffy' states for character set in CCODE command"]
      | otherwise                                   = pure xs

-- | Parses a metadata value mutation.
ccodeCharacterState :: (MonadParsec e s m, Token s ~ Char) => m CharacterState
ccodeCharacterState = choice states
  where
    state c = symbol (char c)
    states  = [ Additive    <$   state '+'
              , NonAdditive <$   state '-'
              , Active      <$   state '['
              , NonActive   <$   state ']'
              , Sankoff     <$   state '('
              , NonSankoff  <$   state ')'
              , Weight      <$> (state '/' *> symbol (flexibleNonNegativeInt "weight value"))
              , Steps       <$> (state '=' *> symbol (flexibleNonNegativeInt "step value"  ))
              ]

-- | A deviation from the strict specification to compensate for human tendncies
--   to close parens and braces even though that is incorrect according to the
--   grammar specifaction.
allowIncorrectSuffixes :: (MonadParsec e s m, Token s ~ Char) => m (Maybe Char)
allowIncorrectSuffixes = optional . symbol $ oneOf "])"
