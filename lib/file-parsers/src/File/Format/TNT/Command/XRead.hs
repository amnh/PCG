----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.TNT.Command.XRead
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Parser for the XREAD command sepecifying the collection of the taxa set
-- and their corresponding sequences. Sequences are well typed and may be
-- specified as contiguous segments of character types.
-----------------------------------------------------------------------------

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

module File.Format.TNT.Command.XRead
  ( xreadCommand
  , discreteSequence
  , dnaSequence
  , proteinSequence
  , xreadHeader
  ) where


import           Data.Bifunctor           (second)
import           Data.Bits
import           Data.CaseInsensitive     (FoldCase)
import           Data.Char                (isSpace)
import           Data.DList               (DList, append)
import qualified Data.DList               as DL (concat, fromList)
import           Data.Foldable            (toList)
import           Data.Functor             (($>))
import           Data.Key                 ((!))
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty       as NE (filter, fromList, length)
import           Data.List.Utility
import           Data.Map                 (assocs, insertWith, lookup)
import           Data.Maybe               (catMaybes, mapMaybe)
import           Data.Traversable
import           File.Format.TNT.Internal
import           Prelude                  hiding (lookup)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Custom


-- |
-- Parses an XREAD command. Correctly validates for taxa count and character
-- sequence length. Produces one or more taxa sequences.
xreadCommand :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m XRead
xreadCommand = xreadValidation =<< xreadDefinition
  where
    xreadDefinition :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m (Int, Int, NonEmpty TaxonInfo)
    xreadDefinition = uncurry (,,) <$> xreadPreamble <*> xreadSequences <* symbol (char ';')

    xreadValidation :: (MonadFail m, MonadParsec e s m {- , Token s ~ Char -}) => (Int, Int, NonEmpty TaxonInfo) -> m XRead
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


-- |
-- Consumes everything in the XREAD command prior to the taxa sequences.
-- Produces the expected taxa count and the length of the character sequences.
xreadPreamble :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m (Int, Int)
xreadPreamble = xreadHeader *> ((,) <$> xreadCharCount <*> xreadTaxaCount)


-- |
-- The superflous information of an XREAD command.
-- Consumes the XREAD string identifier and zero or more comments
-- preceding the taxa count and character cound parameters
xreadHeader :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m ()
xreadHeader =  symbol (keyword "xread" 2)
            *> many simpleComment
            $> ()
  where
    simpleComment = delimiter *> anythingTill delimiter <* symbol delimiter
      where
        delimiter = char '\''


-- |
-- The number of taxa present in the XREAD command.
-- __Naturally__ this number must be a positive integer.
xreadTaxaCount :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m Int
xreadTaxaCount = symbol $ flexiblePositiveInt "taxa count"


-- |
-- The number of characters in a taxon sequence for this XREAD command.
-- __Naturally__ this number must be a non-negative integer.
xreadCharCount :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m Int
xreadCharCount = symbol $ flexibleNonNegativeInt "character count"


-- |
-- Reads one or more taxon sequences.
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
xreadSequences :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m (NonEmpty TaxonInfo)
xreadSequences = NE.fromList . deinterleaveTaxa <$> taxonSequence
  where
    deinterleaveTaxa = assocs . fmap toList . foldr f mempty
    f (taxaName, taxonSeq) = insertWith append taxaName (DL.fromList taxonSeq)


-- |
-- Parses a taxon name and sequence of characters for a given character.
-- Character values can be one of 64 states ordered @[0..9,A..Z,a..z]@ and
-- also the Chars @\'-\'@ & @\'?\'@.
-- Taxon name cannot contain spaces or the @\';\'@ character.
taxonSequence :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m (NonEmpty TaxonInfo)
taxonSequence = NE.fromList . toList . DL.concat <$> some taxonSequenceSegment


-- |
-- Parses the taxon name prepending a taxon sequence. A taxon name cannot begin
-- with with the prefix '"&["' as this make the taxon name ambiguous with a
-- character type segmentation specifcation. Taxon names cannot contain the
-- characters '"().;"' as this makes it impossible to parse other things that I
-- forget. Maybe I'll remember and add that info some day.
taxonName :: (MonadParsec e s m, Token s ~ Char) => m String
taxonName = notFollowedBy openingSequence *> some validNameChar
  where
    validNameChar   = satisfy (\x -> (not . isSpace) x && x `notElem` "(),;")
    openingSequence = char '&' *> char '[' $> ()


-- |
-- Represents a partial taxon sequence. The sequence segment can either have it's
-- character type specified explicitly by a tag or the sequence segment will be
-- interpreted as the default discrete character type.
taxonSequenceSegment :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m (DList TaxonInfo)
taxonSequenceSegment = choice
    [ try taggedInterleaveBlock
    ,    defaultInterleaveBlock
    ]


-- |
-- The default sequence segment type is of discrete characters.
defaultInterleaveBlock :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m (DList TaxonInfo)
defaultInterleaveBlock = discreteSegments


-- Sequence definitions
--------------------------------------------------------------------------------


-- |
-- A sequence segment consisting of real valued literals or \'?\' characters to
-- represent a missing value.
continuousSequence :: (MonadParsec e s m, Token s ~ Char) => m [TntContinuousCharacter]
continuousSequence = many (missingValue <|> presentValue)
  where
    presentValue = Just       <$> double   <* whitespaceInline
    missingValue = Nothing    <$  char '?' <* whitespaceInline


-- |
-- Parses a collection of discrete characters.
-- Intended to be reused as a primitive for other XREAD combinators.
coreDiscreteSequenceThatGetsReused :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m [TntDiscreteCharacter]
coreDiscreteSequenceThatGetsReused = many discreteCharacter
  where
    discreteCharacter  = (ambiguityCharacter <|> singletonCharacter) <* whitespaceInline
    singletonCharacter = bitPack . pure <$> stateToken
    ambiguityCharacter = bitPack <$> (validateAmbiguityGroup =<< withinBraces (many stateToken))
    stateToken         = characterStateChar <* whitespaceInline
    bitPack            = foldr (.|.) zeroBits . mapMaybe (`lookup` deserializeStateDiscrete)
    validateAmbiguityGroup xs
      | null xs    = fail   "An ambiguity group containing no character states was found."
      | hasDupes   = fail $ "An ambiguity group contains duplicate character states: " <> show dupes <> "."
      | hasMissing = fail   "An ambiguity group contains a \"missing data\" character state: '?'."
      | hasGap     = fail   "An ambiguity group contains a \"gap\" character state: '-'."
      | otherwise  = pure xs
      where
        hasDupes   = not $ null dupes
        dupes      = duplicates xs
        hasMissing = '?' `elem` xs && not (isSingleton xs)
        hasGap     = '-' `elem` xs && not (isSingleton xs)


-- |
-- A sequence consisting of characters states which are a prefix of the list:
-- '"?-" <> [\'0\'..\'9\'] <> [\'a\'..\'z\'] <> [\'A\'..\'Z\']'. Ambiguity
-- groups are specified by braces enclosing two or more state values. Results
-- are bitpacked into a 64 bit structure with the ordering specified by
-- 'TntDiscreteCharacter'.
--
-- Remember that you can never have the character literal \'-\' mean gap. It means
-- missing. Why not use \'?\' and just say what you mean? We'll never know.
-- So we substitute gaps for missing in discrete charcters.
discreteSequence :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m [TntDiscreteCharacter]
discreteSequence = substituteGapForMissingBecauseOfReasonsIllNeverUnderstand coreDiscreteSequenceThatGetsReused
  where
    gapOnlyChar = deserializeStateDiscrete ! '-'
    missingChar = deserializeStateDiscrete ! '?'
    substituteGapForMissingBecauseOfReasonsIllNeverUnderstand = fmap (fmap f)
      where
        f c
          | c .&. gapOnlyChar /= zeroBits = missingChar
          | otherwise                     = c


-- |
-- A sequence segment containing dna character states. This sequence segment
-- can contain IUPAC codes which will be converted to abiguity groups, or
-- explicit ambiguity group notation with braces. Ambiguity groups are
-- bitpacked into 8 bit structures with the bit ordering specified by
-- 'TntDnaCharacter'.
dnaSequence :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m [TntDnaCharacter]
dnaSequence = mapM discreteToDna =<< coreDiscreteSequenceThatGetsReused


-- |
-- A sequence segment containing protein character states. This sequence segment
-- can contain IUPAC codes which will be converted to abiguity groups, or
-- explicit ambiguity group notation with braces. Ambiguity groups are bitpacked
-- into 8 bit structures with the bit ordering specified by 'TntProteinCharacter'.
proteinSequence :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m [TntProteinCharacter]
proteinSequence = mapM discreteToProtein =<< coreDiscreteSequenceThatGetsReused


-- Sequence normalization & support
--------------------------------------------------------------------------------


-- |
-- A conversion function from a discrete character ambiguity group to a dna
-- character ambiguity group. The mapping is not injective and unmatched
-- discrete character values will result in a parse error being raised.
discreteToDna :: (MonadFail m {- , Token s ~ Char -}) => TntDiscreteCharacter -> m TntDnaCharacter
discreteToDna character = foldl (.|.) zeroBits <$> mapM f flags
  where
    flags = bitsToFlags character
    toDna = (`lookup` deserializeStateDna) . (serializeStateDiscrete !)
    f x   = case toDna x of
              Nothing -> fail $ "The character state '" <> [serializeStateDiscrete ! x] <> "' is not a valid DNA character state."
              Just b  -> pure b


-- |
-- A conversion function from a discrete character ambiguity group to a
-- protein character ambiguity group. The mapping is not injective and
-- unmatched discrete character values will result in a parse error being
-- raised.
discreteToProtein :: (MonadFail m {- , Token s ~ Char -}) => TntDiscreteCharacter -> m TntProteinCharacter
discreteToProtein character = foldl (.|.) zeroBits <$> mapM f flags
  where
    flags     = bitsToFlags character
    toProtein = (`lookup` deserializeStateProtein) . (serializeStateDiscrete !)
    f x       =
        case toProtein x of
          Nothing -> fail $ "The character state '" <> [serializeStateDiscrete ! x] <> "' is not a valid amino acid character state."
          Just b  -> pure b


-- |
-- Represents the terminal character sequence for a chatacter sequence.
-- Nomenclature ambiguities /are fun!/
segmentTerminal :: (MonadParsec e s m, Token s ~ Char) => m ()
segmentTerminal = whitespaceInline *> endOfLine <* whitespace


{-
-- |
-- Takes a 'zeroBits' bit value, a 'Foldable' structure of 'Char's to represent
-- the ordered alphabet and a 'Traversable' structure of 'Char's to be bit
-- encoded. Constructs a bit value with a bit set for each 'Char' in the
-- 'Traversable' structure where the index of the set bit in the bit value is
-- equal to the index of where the 'Char' first occurred in the ordered alphabet
-- 'Foldable' structure.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> toBits (zeroBits :: Word8) "Example" "alex"
-- 102
toBits :: (Bits b, Foldable f, Traversable t) => b -> f Char -> t Char -> b
toBits b xs = foldr (.|.) b . fmap setFlag
  where
    setFlag    = bit . (`getIndex` xs)
    getIndex e = fromJust . snd . foldl f (0,Nothing)
      where
        f a@(i,m) x
          | isJust m  = a
          | e == x    = (i  ,Just i )
          | otherwise = (i+1,Nothing)


-- |
-- Represents a sequence segement tag specification identified by the
-- parameter combinator.
tagIdentifier :: (MonadParsec e s m, Token s ~ Char) => m a -> m ()
tagIdentifier c = symbol (char '&') *> symbol (withinBraces c) $> ()
-}


-- |
-- Represents the parameter combinator within braces.
withinBraces :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
withinBraces = between (f '[') (f ']')
  where
    f c = char c <* whitespaceInline


{-
-- |
-- An append efficient datastructure containing 'TntCharacter's.
-- Used to efficiently concatenat many interleaved taxon sequence segments.
type TntCharacterSegment = DList TntCharacter
-}


-- |
-- Different possible tags specifications for a given sequence segment.
data XReadTag
   = TagContinuous
   | TagDna
   | TagProtein
   | TagNumeric
   | TagGaps
   | TagNoGaps
   | TagTrimHead
   | TagTrimTail
   deriving stock (Eq, Ord, Show)


-- |
-- Different possible parse interpretations derived from a sequence segment tag.
data XReadParseType
   = ParseContinuous
   | ParseDna
   | ParseNumeric
   | ParseProtein
   deriving stock (Eq)


-- |
-- The complete parse interpretation context derived from a sequence segment tag.
data XReadInterpretation
   = XReadInterpretation
   { parseType     :: XReadParseType
   , parseGaps     :: Bool
   , parseTrimHead :: Bool
   , parseTrimTail :: Bool
   } deriving stock (Eq)


-- |
-- Parses a tagged sequence segment. First parses a sequence segment tag.
-- Then validates that mutually exclusive tags do not exist. Parses a sequence
-- segment based on the contextual information derived from the sequence segment
-- tag. Applies any transformations to the sequence segment that were specified
-- by the sequence segment tag context.
taggedInterleaveBlock :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m (DList TaxonInfo)
taggedInterleaveBlock = nightmare =<< xreadTags


-- |
-- “Beware that, when ~~fighting monsters~~ writing parsers,
--    you yourself do not become a monster...
--    for when you gaze long into the abyss,
--    the abyss gazes also into you.”
--
-- In all seriousness, interpreting the tags correctly is messy.
nightmare :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => XReadInterpretation -> m (DList TaxonInfo)
nightmare interpretation = transformation <$> segment
  where
    applyGapsToMissings = if not $ parseGaps     interpretation then gapsToMissings else id
    applyTrimHead       = if       parseTrimHead interpretation then trimHead       else id
    applyTrimTail       = if       parseTrimTail interpretation then trimTail       else id
    transformation = applyGapsToMissings . applyTrimTail . applyTrimHead
    segment = case parseType interpretation of
                ParseContinuous -> continuousSegments
                ParseDna        -> dnaSegments
                ParseNumeric    -> discreteSegments
                ParseProtein    -> proteinSegments


continuousSegments, dnaSegments, discreteSegments, proteinSegments :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m (DList TaxonInfo)


-- |
-- Parses one or more continuous sequence segments.
continuousSegments = segmentsOf (fmap Continuous <$> continuousSequence)


-- |
-- Parses one or more dna sequence segments.
dnaSegments        = segmentsOf (fmap Dna        <$> dnaSequence)


-- |
-- Parses one or more discrete sequence segments.
discreteSegments   = segmentsOf (fmap Discrete   <$> discreteSequence)


-- |
-- Parses one or more protein sequence segments.
proteinSegments    = segmentsOf (fmap Protein    <$> proteinSequence)


-- |
-- Parses one or more of the parameter sequence segment combinator.
-- Combinators are separated by "segmentTerminal."
segmentsOf :: (MonadParsec e s m, Token s ~ Char) => m [TntCharacter] -> m (DList TaxonInfo)
segmentsOf seqDef = DL.fromList <$> symbol (segment `sepEndBy1` segmentTerminal)
  where
    segment = (,) <$> (taxonName <* whitespaceInline) <*> seqDef


-- |
-- Replaces gap chracters with missing characters. Correctly handles ambiguity
-- groups.
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


-- |
-- Truncate gap values from the front of the sequence segment.
trimHead :: DList TaxonInfo -> DList TaxonInfo
trimHead = fmap (second f)
  where
    f xs = (toMissing <$> gaps) <> chars
      where
        (gaps,chars) = span isGap xs


-- |
-- Truncate gap values from the end of the sequence segment.
trimTail :: DList TaxonInfo -> DList TaxonInfo
trimTail = fmap (second f)
  where
    f xs = reverse $ (toMissing <$> gaps) <> chars
      where
        (gaps,chars) = span isGap $ reverse xs


-- |
-- Test if a given sequence character value is a gap value.
isGap :: TntCharacter -> Bool
isGap (Dna      x) | deserializeStateDna      ! '-' == x = True
isGap (Discrete x) | deserializeStateDiscrete ! '-' == x = True
isGap (Protein  x) | deserializeStateProtein  ! '-' == x = True
isGap _            = False


-- |
-- Overwrite given seuqence character value with the missing character value.
toMissing :: TntCharacter -> TntCharacter
toMissing Dna     {} = Dna      $ deserializeStateDna      ! '?'
toMissing Discrete{} = Discrete $ deserializeStateDiscrete ! '?'
toMissing Protein {} = Protein  $ deserializeStateProtein  ! '?'
toMissing e          = e


-- |
-- Reads a sequence segment tag containing one or more parse interpretation
-- identifiers. Validates that neither duplicate nor mutually exclusive
-- identifiers exist. Returns the contextual information for interpreting the
-- sequence segments following the tag.
xreadTags :: (FoldCase (Tokens s), MonadFail m, MonadParsec e s m, Token s ~ Char) => m XReadInterpretation
xreadTags = validateXReadTags =<< xreadTagsDef
  where
    xreadTagsDef = symbol (char '&') *> symbol (withinBraces tags)

    tags         = NE.fromList <$> (tagOptions `sepBy1` whitespaceInline)

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

    validateXReadTags :: (MonadFail m {- , Token s ~ Char -}) => NonEmpty XReadTag -> m XReadInterpretation
    validateXReadTags xs
      | not (null dupes)                      = fail $ "You got duplicate tags man! Like only one's allowed. For realz! " <> show dupes
      | manyTags allTypeTags                  = fail $ "You can't have multiple character type tags: " <> show allTypeTags
      | manyTags allGapTags                   = fail $ "You can't have multiple gap specification tags: " <> show allGapTags
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

