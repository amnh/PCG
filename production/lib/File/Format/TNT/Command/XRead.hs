{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Command.XRead where

{-- TODO:
  - Robust tests
  - Good documentation
  - Organize this jumbled monolith
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
    deinterleaveTaxa = assocs . fmap toList . foldr f mempty
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
taxonSequenceSegment = choice [ try taggedInterleaveBlock
                              ,    defaultInterleaveBlock
                              ]

defaultInterleaveBlock :: MonadParsec s m Char => m (DList TaxonInfo)
defaultInterleaveBlock = discreteSegments

-- Sequence definitions
--------------------------------------------------------------------------------

continuousSequence :: MonadParsec s m Char => m [TntContinuousCharacter]
continuousSequence = many (missingValue <|> presentValue)
  where
    presentValue = Just       <$> double   <* whitespaceInline
    missingValue = Nothing    <$  char '?' <* whitespaceInline


discreteSequence :: MonadParsec s m Char => m [TntDiscreteCharacter]
discreteSequence = many discreteCharacter
  where
    discreteCharacter         = (ambiguityCharacter <|> singletonCharacter) <* whitespaceInline
    singletonCharacter        = bitPack . pure <$> stateToken
    ambiguityCharacter        = bitPack <$> (validateAmbiguityGroup =<< withinBraces (many stateToken))
    stateToken                = characterStateChar <* whitespaceInline
    bitPack                   = foldr (.|.) zeroBits . catMaybes . fmap (`lookup` deserializeStateDiscrete)
    validateAmbiguityGroup xs
      | null xs    = fail   "An ambiguity group containing no character states was found."
      | hasDupes   = fail $ "An ambiguity group contains duplicate character states: " ++ show dupes ++ "."
      | hasMissing = fail   "An ambiguity group contains a \"missing data\" character state: '?'."
      | hasGap     = fail   "An ambiguity group contains a \"gap\" character state: '-'."
      | otherwise  = pure xs
      where
        hasDupes   = not $ null dupes
        dupes      = duplicates xs
        hasMissing = '?' `elem` xs && not (isSingleton xs)
        hasGap     = '-' `elem` xs && not (isSingleton xs)

dnaSequence :: MonadParsec s m Char => m [TntDnaCharacter]
dnaSequence = mapM discreteToDna =<< discreteSequence

proteinSequence :: MonadParsec s m Char => m [TntProteinCharacter]
proteinSequence = mapM discreteToProtein =<< discreteSequence

-- Sequence normalization & support
--------------------------------------------------------------------------------

discreteToDna :: MonadParsec s m Char => TntDiscreteCharacter -> m TntDnaCharacter
discreteToDna character = foldl (.|.) zeroBits <$> mapM f flags
  where
    flags = bitsToFlags character
    toDna = (`lookup` deserializeStateDna) . (serializeStateDiscrete !)
    f x   = case toDna x of
              Nothing -> fail $ "The character state '" ++ [serializeStateDiscrete ! x] ++ "' is not a valid DNA character state." 
              Just b  -> pure b

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




type TntCharacterSegment = DList TntCharacter

data XReadTag
   = TagContinuous
   | TagDna
   | TagProtein
   | TagNumeric
   | TagGaps
   | TagNoGaps
   | TagTrimHead
   | TagTrimTail
   deriving (Eq,Ord,Show)

data XReadParseType
   = ParseContinuous
   | ParseDna
   | ParseNumeric
   | ParseProtein
   deriving (Eq)

data XReadInterpretation
   = XReadInterpretation
   { parseType     :: XReadParseType
   , parseGaps     :: Bool
   , parseTrimHead :: Bool
   , parseTrimTail :: Bool
   } deriving (Eq)

taggedInterleaveBlock :: MonadParsec s m Char => m (DList TaxonInfo)
taggedInterleaveBlock = nightmare =<< xreadTags

-- | “Beware that, when ~~fighting monsters~~ writing parsers,
--    you yourself do not become a monster...
--    for when you gaze long into the abyss,
--    the abyss gazes also into you.”
nightmare :: MonadParsec s m Char => XReadInterpretation -> m (DList TaxonInfo)
nightmare interpretation = (gapsToMissings . trimTail . trimHead) <$> segment
  where
    segment = case parseType interpretation of
                ParseContinuous -> continuousSegments
                ParseDna        -> dnaSegments
                ParseNumeric    -> discreteSegments
                ParseProtein    -> proteinSegments

continuousSegments :: MonadParsec s m Char => m (DList TaxonInfo)
continuousSegments = segmentsOf (fmap Continuous <$> continuousSequence)

dnaSegments :: MonadParsec s m Char => m (DList TaxonInfo)
dnaSegments = segmentsOf (fmap Dna <$> dnaSequence)

discreteSegments :: MonadParsec s m Char => m (DList TaxonInfo)
discreteSegments = segmentsOf (fmap Discrete <$> discreteSequence)

proteinSegments :: MonadParsec s m Char => m (DList TaxonInfo)
proteinSegments = segmentsOf (fmap Protein <$> proteinSequence)

{-
discreteSegments :: MonadParsec s m Char => m (DList TaxonInfo)
discreteSegments = DL.fromList <$> symbol (discreteSegment `sepEndBy1` segmentTerminal)
  where
    discreteSegment = (,) <$> (taxonName <* whitespaceInline) <*> (fmap Discrete <$> discreteSequence)
-}

segmentsOf :: MonadParsec s m Char => m [TntCharacter] -> m (DList TaxonInfo)
segmentsOf seqDef = DL.fromList <$> symbol (segment `sepEndBy1` segmentTerminal)
  where
    segment = (,) <$> (taxonName <* whitespaceInline) <*> seqDef

gapsToMissings :: DList TaxonInfo -> DList TaxonInfo
gapsToMissings = fmap (second (fmap gapToMissing))
  where
    gapToMissing e@(Continuous _ ) = e
    gapToMissing e@(Dna x)
      | x `testBit` gapBit = Dna missing
      | otherwise          = e
      where
        gapBit  = findFirstSet $ deserializeStateDna ! '-'
        missing = deserializeStateDna ! '?'
    gapToMissing e@(Discrete x)
      | x `testBit` gapBit = Discrete missing
      | otherwise          = e
      where
        gapBit  = findFirstSet $ deserializeStateDiscrete ! '-'
        missing = deserializeStateDiscrete ! '?'
    gapToMissing e@(Protein x)
      | x `testBit` gapBit = Protein missing
      | otherwise          = e
      where
        gapBit  = findFirstSet $ deserializeStateProtein ! '-'
        missing = deserializeStateProtein ! '?'

trimHead :: DList TaxonInfo -> DList TaxonInfo
trimHead = fmap (second f)
  where
    f xs = (toMissing <$> gaps) ++ chars
      where
        (gaps,chars) = span isGap xs

trimTail :: DList TaxonInfo -> DList TaxonInfo
trimTail = fmap (second f)
  where
    f xs = reverse $ (toMissing <$> gaps) ++ chars
      where
        (gaps,chars) = span isGap $ reverse xs

isGap (Dna      x) | deserializeStateDna      ! '-' == x = True 
isGap (Discrete x) | deserializeStateDiscrete ! '-' == x = True 
isGap (Protein  x) | deserializeStateProtein  ! '-' == x = True 
isGap _ = False

toMissing (Dna      x) = Dna      $ deserializeStateDna      ! '?'
toMissing (Discrete x) = Discrete $ deserializeStateDiscrete ! '?'
toMissing (Protein  x) = Protein  $ deserializeStateProtein  ! '?'
toMissing e = e

xreadTags :: MonadParsec s m Char => m XReadInterpretation
xreadTags = validateXReadTags =<< xreadTagsDef
  where
    xreadTagsDef :: MonadParsec s m Char => m (NonEmpty XReadTag)
    xreadTagsDef = symbol (char '&') *> symbol (withinBraces tags)
    tags         :: MonadParsec s m Char => m (NonEmpty XReadTag)
    tags         = NE.fromList <$> (tagOptions `sepBy1` whitespaceInline)
    tagOptions   :: MonadParsec s m Char => m XReadTag
    tagOptions   = choice
                 [      keyword "continuous" 4  $> TagContinuous
                 ,      keyword "dna"        3  $> TagDna
                 ,      keyword "protein"    4  $> TagProtein
                 , try (keyword "numeric"    3) $> TagNumeric
                 ,      keyword "gaps"       3  $> TagGaps
                 ,      keyword "nogaps"     5  $> TagNoGaps
                 , try (keyword "trimhead"   5) $> TagTrimHead
                 ,      keyword "trimtail"   5  $> TagTrimTail
                 ]
    validateXReadTags :: MonadParsec s m Char => NonEmpty XReadTag -> m XReadInterpretation
    validateXReadTags xs
      | not (null dupes)                      = fail $ "You got duplicate tags man! Like only one's allowed. For realz! " ++ show dupes
      | manyTags allTypeTags                  = fail $ "You can't have multiple character type tags: " ++ show allTypeTags
      | manyTags allGapTags                   = fail $ "You can't have multiple gap specification tags: " ++ show allGapTags
      | isContinuous && not (null allGapTags) = fail   "You can't have gaps or nogaps specified for continuous data"
      | isNumeric    && not (null allGapTags) = fail   "You can't have gaps or nogaps specified for numeric data"
      | not pGaps && (pTrimHead || pTrimTail) = fail   "You can't have trimhead or trimTail along with nogaps"
      | otherwise                             = pure $ XReadInterpretation pType pGaps pTrimHead pTrimTail
      where
        dupes        = duplicates xs
        allTypeTags  = filter (`elem` typeTags) $ toList xs
        allGapTags   = filter (`elem` gapTags ) $ toList xs
        typeTags     = [TagContinuous, TagDna, TagProtein, TagNumeric]
        gapTags      = [TagGaps, TagNoGaps]
        manyTags     = (>1) . length
        isContinuous = TagContinuous `elem` allTypeTags
        isNumeric    = TagNumeric    `elem` allTypeTags
        pType        = case allTypeTags of
                         [TagContinuous] -> ParseContinuous
                         [TagDna       ] -> ParseDna
                         [TagProtein   ] -> ParseProtein
                         _               -> ParseNumeric
        pGaps        = case allGapTags of
                         [TagNoGaps] -> False
                         _           -> True
        pTrimHead    = TagTrimHead `elem` xs
        pTrimTail    = TagTrimTail `elem` xs

