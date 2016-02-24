{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Command.CNames where

import           Data.Bifunctor           (second)
import           Data.Char                (isSpace)
--import           Data.IntSet              (IntSet, filter, singleton)
--import qualified Data.IntSet        as IS (fromList)
import           Data.Foldable            (toList)
import           Data.IntMap              (IntMap,singleton,insertWith)
import qualified Data.IntMap        as IM (fromList)
import           Data.List                (isSuffixOf,intersperse,sort,sortBy)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (filter,fromList,length)
--import           Data.Map.Strict          (Map,insertWith)
--import qualified Data.Map.Strict    as M  (toList)
import           Data.Maybe               (catMaybes)
import           Data.Ord                 (comparing)
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Lexer    (integer,number)
import           Text.Megaparsec.Prim     (MonadParsec)

-- | Parses an TREAD command. Correctly validates for taxa count
-- and character sequence length. Produces one or more taxa sequences.
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
        isSingleton [x] = True
        isSingleton _   = False
        mapBuild x = insertWith f (sequenceIndex x) [x]
          where
            f [new] old = new:old
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

{-
      | null errors = pure $ XRead charCount taxaCount taxaSeqs
      | otherwise   = fails errors
      where
        errors = catMaybes [taxaCountError, charCountError]
        taxaCountError = let taxaLength = NE.length taxaSeqs
                         in if taxaCount == taxaLength
                            then Nothing
                            else Just $ concat
                              [ "The number of taxa specified ("
                              , show taxaCount
                              , ") does not match the number of taxa found ("
                              , show $ length taxaSeqs
                              , ")"
                              ]
        charCountError = case NE.filter ((/= charCount) . snd) . fmap (second length) $ taxaSeqs of
                           [] -> Nothing
                           xs -> Just $ concat
                              [ "The number of characters specified ("
                              , show charCount
                              , ") does not match the number of chararacters found for the following taxa:\n"
                              , unlines $ prettyPrint <$> xs
                              ]                            
        prettyPrint (name, count) = concat ["\t",show name," found (",show count,") characters"]
-}

{-
-- | Consumes everything in the XREAD command prior to the taxa sequences.
-- Produces the expected taxa count and the length of the character sequences.
xreadPreamble :: MonadParsec s m Char => m (Int, Int)
xreadPreamble = treadHeader *> ((,) <$> xreadCharCount <*> xreadTaxaCount)
-}

-- | The superflous information of an CNAMES command.
-- Consumes the XREAD string identifier and zero or more comments
-- preceeding the taxa count and character cound parameters
cnamesHeader :: MonadParsec s m Char => m ()
cnamesHeader = symbol (keyword "cnames" 2)
            *> many (symbol simpleComment)
            *> pure ()
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
                     <$> symbol nonNegInt
                     <*> lineToken
                     <*> many lineToken



