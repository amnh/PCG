{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Command.CNames where

import           Data.Foldable            (toList)
import           Data.Functor             (($>))
import           Data.IntMap              (insertWith)
import           Data.List                (sort,sortBy)
import qualified Data.List.NonEmpty as NE (fromList)
import           Data.List.Utility
import           Data.Ord                 (comparing)
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Prim     (MonadParsec)

-- | Parses an CNAMES command. Correctly validates that there is no
--   duplicate naming for a single character index in the sequence.
cnamesCommand :: MonadParsec s m Char => m CNames
cnamesCommand = cnamesValidation =<< cnamesDefinition
  where
    cnamesDefinition :: MonadParsec s m Char => m CNames
    cnamesDefinition = symbol cnamesHeader
                    *> symbol cnamesBody
                    <* symbol (char ';')

    -- | Make sure indicies are unique
    cnamesValidation :: MonadParsec s m Char => CNames -> m CNames
    cnamesValidation cnames
      | not (null duplicateIndexErrors) = fails duplicateIndexErrors
      | otherwise                       = pure cnames
      where
        duplicateIndexErrors = duplicateIndexMessages cnames

duplicateIndexMessages :: CNames -> [String]
duplicateIndexMessages cnames = duplicateIndexErrors
  where
    duplicateIndicies   = filter (not.isSingleton) . toList $ foldr mapBuild mempty cnames
      where
        mapBuild x = insertWith f (sequenceIndex x) [x]
          where
            f [new] old = new:old
            f _     old = old -- TODO: Check this reasoning in "catch-all" case
    duplicateIndexErrors = toList $ fmap getErrorMessage duplicateIndicies
      where
        getErrorMessage xs = unwords
                           [ "Multiple character names defined for the index '" ++ show indexValue ++ "',"
                           , "expecting at most one character name for each index."
                           , "The following character names were found for index '" ++ show indexValue ++ "':"
                           , show (sort nameValues)
                           ]
          where
            indexValue   = sequenceIndex $ head xs
            nameValues   = characterId <$> xs

-- |  Consumes the XREAD string identifier and zero or more comments
--    preceeding the taxa count and character cound parameters
cnamesHeader :: MonadParsec s m Char => m ()
cnamesHeader = symbol (keyword "cnames" 2)
            *> many (symbol simpleComment)
            $> ()
  where
    simpleComment = delimiter *> anythingTill delimiter <* symbol delimiter
      where
        delimiter = char '\''
     
cnamesBody :: MonadParsec s m Char => m CNames
cnamesBody = NE.fromList . normalize <$> some cnamesStateName
  where
    normalize = sortBy (comparing sequenceIndex)

cnamesStateName :: MonadParsec s m Char => m CharacterName
cnamesStateName = symbol (char '{') *> cnameCharacterName <* symbol terminator
  where
    terminator :: MonadParsec s m Char => m Char
    terminator         = char ';'
    lineToken :: MonadParsec s m Char => m String
    lineToken          = symbol $ somethingTill (inlineSpaceChar <|> terminator)
    cnameCharacterName = CharacterName
                     <$> symbol (flexibleNonNegativeInt "character name's sequence index")
                     <*> lineToken
                     <*> many lineToken
