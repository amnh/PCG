{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Command.CCode where

import           Data.IntMap              (IntMap,insertWith)
import qualified Data.IntMap        as IM (lookup)
import qualified Data.List.NonEmpty as NE (fromList)
import           Data.Maybe               (fromMaybe)
import           Data.Vector              (Vector,generate)
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Prim     (MonadParsec)

-- | Parses a CCODE command that consists of:
--
--  * A single specification of the character state change
--
--  * One or more character indicies or index ranges of affected characters
ccodeCommand :: MonadParsec s m Char => m CCode
ccodeCommand = ccodeHeader *> ccodeBody <* symbol (char ';')
  where
    ccodeBody = ccodeAdditive
            <|> ccodeNonAdditive
            <|> ccodeActive
            <|> ccodeNonActive
            <|> ccodeSankoff
            <|> ccodeNonSankoff
            <|> ccodeWeight
            <|> ccodeSteps

-- | Consumes the superflous heading for a CCODE command.
ccodeHeader :: MonadParsec s m Char => m ()
ccodeHeader = symbol $ keyword "ccode" 2

-- | A Uitility function for creating 'CCode' combinators
ccodeMetaChange :: MonadParsec s m Char => Char -> CharacterState -> m CCode
ccodeMetaChange c s = do
    _ <- symbol (char c)
    i <- NE.fromList <$> some characterIndicies
    pure $ CCode s i

-- | Parses a _Additive_ specification `CCode`
ccodeAdditive    :: MonadParsec s m Char => m CCode
ccodeAdditive    = ccodeMetaChange '+' Additive

-- | Parses a _Non-Additive_ specification `CCode`
ccodeNonAdditive :: MonadParsec s m Char => m CCode
ccodeNonAdditive = ccodeMetaChange '-' NonAdditive

-- | Parses a _Active_ specification `CCode`
ccodeActive      :: MonadParsec s m Char => m CCode
ccodeActive      = ccodeMetaChange '[' Active <* allowIncorrectSuffix ']'

-- | Parses a _Non-Active_ specification `CCode`
ccodeNonActive   :: MonadParsec s m Char => m CCode
ccodeNonActive   = ccodeMetaChange ']' NonActive

-- | Parses a _Sankoff_ specification `CCode`
ccodeSankoff     :: MonadParsec s m Char => m CCode
ccodeSankoff     = ccodeMetaChange '(' Sankoff <* allowIncorrectSuffix ')'

-- | Parses a _Non-Sankoff_ specification `CCode`
ccodeNonSankoff :: MonadParsec s m Char => m CCode
ccodeNonSankoff  = ccodeMetaChange ')' NonSankoff

-- | Parses a _weight_ specification `CCode`
ccodeWeight :: MonadParsec s m Char => m CCode
ccodeWeight = do
    _ <- symbol (char '/')
    w <- Weight <$> symbol (flexibleNonNegativeInt "weight value")
    i <- NE.fromList <$> some characterIndicies
    pure $ CCode w i

-- | Parses a _step_ specification `CCode`
ccodeSteps :: MonadParsec s m Char => m CCode
ccodeSteps = do
    _ <- symbol (char '=')
    w <- Steps <$> symbol (flexibleNonNegativeInt "step value")
    i <- NE.fromList <$> some characterIndicies
    pure $ CCode w i

allowIncorrectSuffix :: MonadParsec s m Char => Char -> m (Maybe Char)
allowIncorrectSuffix = optional . symbol . char
