{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Command.XRead where

{-- TODO:
  - Robust tests
  - Good documentation
  - Deinterleave function with DList construction
  -}

import           Data.Bifunctor           (second)
import           Data.Bits
import           Data.Char                (isSpace)
import           Data.DList               (DList,append)
import qualified Data.DList         as DL (concat,fromList)
import           Data.Foldable            (toList)
import           Data.Functor             (($>))
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (filter,fromList,length)
import           Data.Map.Strict          (Map,assocs,insertWith)
import           Data.Maybe               (catMaybes,fromJust,isJust)
import           Data.Word                (Word8,Word64)
import           File.Format.TNT.Internal
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
    numericSegment       = (,) <$> (taxonName <* whitespaceInline) <*> many numericCharacter
    numericCharacter     = Discrete <$> (ambiguityCharacter <|> singletonCharacter) <* whitespaceInline
    singletonCharacter   = bitPack . pure <$> stateToken
    ambiguityCharacter   = bitPack <$> withinBraces (some stateToken)
    stateToken           = characterStateChar <* whitespaceInline
    bitPack              = toBits (zeroBits :: Word64) characterStateValues

dnaInterleaveBlock :: MonadParsec s m Char => m (DList TaxonInfo)
dnaInterleaveBlock = dnaIdentifierTag *> (DL.fromList <$> symbol (dnaSegment `sepEndBy1` segmentTerminal))
  where
    dnaIdentifierTag   = tagIdentifier $ keyword "dna" 3 -- use keyword, handles lookAhead after the 'a'
    dnaSegment         = (,) <$> (taxonName <* whitespaceInline) <*> many dnaCharacter
    dnaCharacterValues = "AGCT-?"
    dnaCharacter       = Dna <$> (singletonCharacter <|> ambiguityCharacter) <* whitespaceInline
    singletonCharacter = bitPack . pure <$> stateToken
    ambiguityCharacter = bitPack <$> withinBraces (some stateToken)
    stateToken         = oneOf' dnaCharacterValues <* whitespaceInline
    bitPack            = toBits (zeroBits :: Word8) dnaCharacterValues

defaultInterleaveBlock :: MonadParsec s m Char => m (DList TaxonInfo)
defaultInterleaveBlock = numericSegments

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
withinBraces = between (char '[' <* whitespaceInline) (char ']' <* whitespaceInline)
