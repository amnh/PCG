{-# LANGUAGE FlexibleContexts #-}
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
ccodeCommand :: MonadParsec s m Char => m CCode
ccodeCommand = ccodeHeader *> nonEmpty ccodeAugment <* symbol (char ';')

ccodeHeader :: MonadParsec s m Char => m ()
ccodeHeader = symbol $ keyword "ccode" 2

ccodeAugment :: MonadParsec s m Char => m CCodeAugment
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
      | otherwise                                   = pure xs

ccodeCharacterState :: MonadParsec s m Char => m CharacterState
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

allowIncorrectSuffixes :: MonadParsec s m Char => m (Maybe Char)
allowIncorrectSuffixes = optional . symbol $ oneOf "])"
