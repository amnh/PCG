----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TNT.Command.CNames
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Parser for the CNames command.
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module File.Format.TNT.Command.CNames
  ( cnamesCommand
  , duplicateIndexMessages
  , cnamesHeader
  , cnamesStateName
  ) where

import           Data.CaseInsensitive     (FoldCase)
import           Data.Functor             (void)
import           Data.Foldable            (toList)
import           Data.Functor             (($>))
import           Data.IntMap              (insertWith)
import           Data.List                (sort, sortOn)
import qualified Data.List.NonEmpty       as NE (fromList)
import           Data.List.Utility
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Custom


-- |
-- Parses an CNAMES command. Correctly validates that there is no
-- duplicate naming for a single character index in the sequence.
cnamesCommand :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m CNames
cnamesCommand = cnamesValidation =<< cnamesDefinition
  where
    cnamesDefinition = symbol cnamesHeader
                    *> symbol cnamesBody
                    <* symbol (char ';')

    -- Make sure indices are unique
    cnamesValidation :: MonadParsec e s m => CNames -> m CNames
    cnamesValidation cnames
      | not (null duplicateIndexErrors) = fails duplicateIndexErrors
      | otherwise                       = pure cnames
      where
        duplicateIndexErrors = duplicateIndexMessages cnames


-- |
-- Validation functions to ensure that character names are not specified
-- for a given character index multiple times.
duplicateIndexMessages :: CNames -> [String]
duplicateIndexMessages cnames = duplicateIndexErrors
  where
    duplicateIndicies   = filter (not . isSingleton) . toList $ foldr mapBuild mempty cnames
      where
        mapBuild x = insertWith (<>) (sequenceIndex x) [x]

    duplicateIndexErrors = toList $ fmap getErrorMessage duplicateIndicies
      where
        getErrorMessage xs = unwords
                           [ "Multiple character names defined for the index '" <> show indexValue <> "',"
                           , "expecting at most one character name for each index."
                           , "The following character names were found for index '" <> show indexValue <> "':"
                           , show (sort nameValues)
                           ]
          where
            indexValue   = sequenceIndex $ head xs
            nameValues   = characterId <$> xs


-- |
-- Consumes the CNAMES string identifier and zero or more comments
-- preceding the taxa count and character cound parameters
cnamesHeader :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m ()
cnamesHeader = symbol (keyword "cnames" 2)
            *> many (symbol simpleComment)
            $> ()
  where
    simpleComment = delimiter *> anythingTill delimiter <* symbol delimiter
      where
        delimiter = char '\''


-- |
-- Parses the body of a CNAMES command and returns a list of character names
-- sorted in ascending order of the character index that the names correspond
-- to.
cnamesBody :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m CNames
cnamesBody = NE.fromList . normalize <$> some cnamesStateName
  where
    normalize = sortOn sequenceIndex


-- |
-- Parses an individual CNAMES character name specification for a character
-- index allong with the character name and names of the state values for the
-- character.
cnamesStateName :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m CharacterName
cnamesStateName = symbol (char '{') *> cnameCharacterName <* symbol terminator
  where
    terminator :: (MonadParsec e s m, Token s ~ Char) => m Char
    terminator         = char ';'
    lineToken :: (MonadParsec e s m, Token s ~ Char) => m String
    lineToken          = symbol $ somethingTill (hspace1 <|> void terminator)
    cnameCharacterName = CharacterName
                     <$> symbol (flexibleNonNegativeInt "character name's sequence index")
                     <*> lineToken
                     <*> many lineToken
