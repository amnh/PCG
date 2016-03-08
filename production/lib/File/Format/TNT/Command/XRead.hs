{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Command.XRead where

{-- TODO:
  - Robust tests
  - Good documentation
  - Deinterleave function with DList construction
  -}

import           Control.Monad            ((<=<))
import           Data.Bifunctor           (second)
import           Data.Bits
import           Data.Char                (isSpace)
import           Data.DList               (DList,append)
import qualified Data.DList         as DL (concat,fromList)
import           Data.Foldable            (toList)
import           Data.Functor             (($>))
import           Data.Key                 ((!))
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (filter,fromList,length)
import           Data.List.Utility
import           Data.Map                 (Map,assocs,insertWith,lookup)
import           Data.Maybe               (catMaybes,fromJust,isJust)
import           Data.Traversable
import           Data.Word                (Word8,Word64)
import           File.Format.TNT.Internal
import           Prelude           hiding (lookup)
import           Text.Megaparsec
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Prim     (MonadParsec)

-- | Parses an XREAD command. Correctly validates for taxa count
-- and character sequence length. Produces one or more taxa sequences.
xreadCommand :: MonadParsec s m Char => m XRead
xreadCommand = xreadValidation =<< xreadDefinition
  where
    xreadDefinition :: MonadParsec s m Char => m (Int, Int, NonEmpty TaxonInfo)
    xreadDefinition = uncurry (,,) <$> xreadPreamble <*> xreadSequences <* symbol (char ';')

    xreadValidation :: MonadParsec s m Char => (Int, Int, NonEmpty TaxonInfo) -> m XRead
    xreadValidation (charCount, taxaCount, taxaSeqs)
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
        prettyPrint (name, num) = concat ["\t",show name," found (",show num,") characters"]

-- | Consumes everything in the XREAD command prior to the taxa sequences.
-- Produces the expected taxa count and the length of the character sequences.
xreadPreamble :: MonadParsec s m Char => m (Int, Int)
xreadPreamble = xreadHeader *> ((,) <$> xreadCharCount <*> xreadTaxaCount)

-- | The superflous information of an XREAD command.
-- Consumes the XREAD string identifier and zero or more comments
-- preceeding the taxa count and character cound parameters
xreadHeader :: MonadParsec s m Char => m ()
xreadHeader =  symbol (keyword "xread" 2)
            *> many simpleComment
            *> pure ()
  where
    simpleComment = delimiter *> anythingTill delimiter <* symbol delimiter
      where
        delimiter = char '\''

-- | The number of taxa present in the XREAD command.
-- __Naturally__ this number must be a positive integer.
xreadTaxaCount :: MonadParsec s m Char => m Int
xreadTaxaCount = symbol $ flexiblePositiveInt "taxa count" 

-- | The number of characters in a taxon sequence for this XREAD command.
-- __Naturally__ this number must be a non-negative integer.
xreadCharCount :: MonadParsec s m Char => m Int
xreadCharCount = symbol $ flexibleNonNegativeInt "character count"

-- | Reads one or more taxon sequences.
-- Performs deinterleaving of identically named taxon sequences. 
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parse xreadSequences "" "taxonOne GATACA\ntaxonTwo GGAATT"
-- Right [ ("taxonOne", "GATACA")
--       , ("taxonTwo", "GGAATT")
--       ]
--
-- Interleaved usage:
--
-- >>> parse xreadSequences "" "taxonOne GATACA\ntaxonTwo GGAATT\ntaxonOne ACATAG\ntaxonTwo CCGATC\n"
-- Right [ ("taxonOne", "GATACAACATAG")
--       , ("taxonTwo", "GGAATTCCGATC")
--       ]
xreadSequences :: MonadParsec s m Char => m (NonEmpty TaxonInfo)
xreadSequences = NE.fromList . deinterleaveTaxa <$> taxonSequence
  where
--    deinterleaveTaxa :: [TaxonInfo] -> [TaxonInfo]
    deinterleaveTaxa = assocs . fmap toList . foldr f mempty
      where
--        f :: TaxonInfo -> Map TaxonName (DList String) -> Map TaxonName (DList String)
        f (taxonName, taxonSeq) = insertWith append taxonName (DL.fromList taxonSeq)

-- | Parses a taxon name and sequence of characters for a given character.
-- Character values can be one of 64 states ordered @[0..9,A..Z,a..z]@ and also the Chars @\'-\'@ & @\'?\'@.
-- Taxon name cannot contain spaces or the @\';\'@ character.
taxonSequence :: MonadParsec s m Char => m (NonEmpty TaxonInfo)
taxonSequence = NE.fromList . toList . DL.concat <$> some taxonSequenceSegment

taxonName :: MonadParsec s m Char => m String
taxonName = notFollowedBy (string "&[") *> some validNameChar
  where
    validNameChar = satisfy (\x -> (not . isSpace) x && x `notElem` "(),;")

taxonSequenceSegment :: MonadParsec s m Char => m (DList TaxonInfo)
taxonSequenceSegment = choice [ try continuousInterleaveBlock
                              , try numericInterleaveBlock
                              , try dnaInterleaveBlock
                              , try proteinInterleaveBlock
                              , defaultInterleaveBlock
                              ]

continuousInterleaveBlock :: MonadParsec s m Char => m (DList TaxonInfo)
continuousInterleaveBlock = continuousIdentifierTag *> (DL.fromList <$> symbol (continuousSegment `sepEndBy1` segmentTerminal))
  where
    continuousIdentifierTag = tagIdentifier $ keyword "continuous" 4
    continuousSegment       = (,) <$> (taxonName <* whitespaceInline) <*> many stateToken
    stateToken              = Continuous <$> double <* whitespaceInline

numericInterleaveBlock :: MonadParsec s m Char => m (DList TaxonInfo)
numericInterleaveBlock = numericIdentifierTag *> numericSegments
  where
    numericIdentifierTag = tagIdentifier $ keyword "numeric" 3

numericSegments :: MonadParsec s m Char => m (DList TaxonInfo)
numericSegments = DL.fromList <$> symbol (numericSegment `sepEndBy1` segmentTerminal)
  where
    numericSegment       = (,) <$> (taxonName <* whitespaceInline) <*> (fmap Discrete <$> discreteSequence)

dnaInterleaveBlock :: MonadParsec s m Char => m (DList TaxonInfo)
dnaInterleaveBlock = dnaIdentifierTag *> (DL.fromList <$> symbol (dnaSegment `sepEndBy1` segmentTerminal))
  where
    dnaIdentifierTag   = tagIdentifier $ keyword "dna" 3 -- use keyword, handles lookAhead after the 'a'
    dnaSegment         = (,) <$> (taxonName <* whitespaceInline) <*> (fmap Dna <$> dnaSequence)

proteinInterleaveBlock :: MonadParsec s m Char => m (DList TaxonInfo)
proteinInterleaveBlock = proteinIdentifierTag *> (DL.fromList <$> symbol (proteinSegment `sepEndBy1` segmentTerminal))
  where
    proteinIdentifierTag   = tagIdentifier $ keyword "protein" 4
    proteinSegment         = (,) <$> (taxonName <* whitespaceInline) <*> (fmap Protein <$> proteinSequence)

defaultInterleaveBlock :: MonadParsec s m Char => m (DList TaxonInfo)
defaultInterleaveBlock = numericSegments

discreteSequence :: MonadParsec s m Char => m [TntDiscreteCharacter]
discreteSequence = many discreteCharacter
  where
    discreteCharacter         = (ambiguityCharacter <|> singletonCharacter) <* whitespaceInline
    singletonCharacter        = bitPack . pure <$> stateToken
    ambiguityCharacter        = bitPack <$> (validateAmbiguityGroup =<< withinBraces (many stateToken))
    stateToken                = characterStateChar <* whitespaceInline
    bitPack                   = foldr (.|.) zeroBits . catMaybes . fmap (`lookup` deserializeStateDiscrete)
    validateAmbiguityGroup [] = fail "An ambiguity group containing no characters was found."
    validateAmbiguityGroup xs
      | null dupes = pure xs
      | otherwise  = fail $ "An ambiguity group contains duplicate characters: " ++ show dupes ++ "."
      where
        dupes = duplicates xs

dnaSequence :: MonadParsec s m Char => m [TntDnaCharacter]
dnaSequence = mapM discreteToDna =<< discreteSequence

discreteToDna :: MonadParsec s m Char => TntDiscreteCharacter -> m TntDnaCharacter
discreteToDna character = foldl (.|.) zeroBits <$> mapM f flags
  where
    flags = bitsToFlags character
    toDna = (`lookup` deserializeStateDna) . (serializeStateDiscrete !)
    f x   = case toDna x of
              Nothing -> fail $ "The character state '" ++ [serializeStateDiscrete ! x] ++ "' is not a valid DNA character state." 
              Just b  -> pure b

proteinSequence :: MonadParsec s m Char => m [TntProteinCharacter]
proteinSequence = mapM discreteToProtein =<< discreteSequence

discreteToProtein :: MonadParsec s m Char => TntDiscreteCharacter -> m TntProteinCharacter
discreteToProtein character = foldl (.|.) zeroBits <$> mapM f flags
  where
    flags     = bitsToFlags character
    toProtein = (`lookup` deserializeStateProtein) . (serializeStateDiscrete !)
    f x       = case toProtein x of
                  Nothing -> fail $ "The character state '" ++ [serializeStateDiscrete ! x] ++ "' is not a valid amino acid character state." 
                  Just b  -> pure b

segmentTerminal :: MonadParsec s m Char => m Char
segmentTerminal = whitespaceInline *> endOfLine <* whitespace

toBits :: (Bits b, Foldable f) => b -> f Char -> String -> b
toBits b xs = foldr (.|.) b . fmap setFlag 
  where
    setFlag    = bit . (`getIndex` xs)
    getIndex e = fromJust . snd . foldl f (0,Nothing)
      where
        f a@(i,m) x
          | isJust m  = a
          | e == x    = (i  ,Just i )
          | otherwise = (i+1,Nothing) 

tagIdentifier :: MonadParsec s m Char => m a -> m ()
tagIdentifier c = symbol (char '&') *> symbol (withinBraces c) $> ()
  
withinBraces :: MonadParsec s m Char => m a -> m a
withinBraces = between (f '[') (f ']')
  where
    f c = char c <* whitespaceInline 
