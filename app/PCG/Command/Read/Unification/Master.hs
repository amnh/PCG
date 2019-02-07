----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Read.Unification.Master
-- Copyright   :  () 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Containing the master command for unifying all input types: tree, metadata, and sequence
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module PCG.Command.Read.Unification.Master
  ( FracturedParseResult(..)
  , masterUnify
  ) where

import           Bio.Character
import           Bio.Character.Decoration.Continuous           hiding (characterName)
import           Bio.Character.Decoration.Discrete             hiding (characterName)
import           Bio.Character.Decoration.Dynamic              hiding (characterName)
import           Bio.Character.Encodable
import           Bio.Metadata.Continuous                       (continuousMetadata)
import           Bio.Sequence                                  hiding (hexmap)
import           Bio.Sequence.Block
import qualified Bio.Sequence.Character                        as CS
import qualified Bio.Sequence.Metadata                         as MD
import           Bio.Graph
import           Bio.Graph.Component
import           Bio.Graph.Node
import           Bio.Graph.ReferenceDAG
import qualified Bio.Graph.ReferenceDAG                        as DAG
import           Bio.Metadata.CharacterName                    hiding (sourceFile)
import           Bio.Metadata.DiscreteWithTCM                  (discreteMetadataWithTCM)
import           Bio.Metadata.Dynamic                          (dynamicMetadataWithTCM)
import           Control.Arrow                                 ((&&&), (***))
import           Control.Lens                                  (over)
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Alphabet
import           Data.Bifunctor                                (first)
import           Data.Coerce                                   (coerce)
import           Data.Default
import           Data.Foldable
import qualified Data.IntMap                                   as IM
import qualified Data.IntSet                                   as IS
import           Data.Key
import           Data.List                                     (transpose, zip5)
import           Data.List.NonEmpty                            (NonEmpty (..))
import qualified Data.List.NonEmpty                            as NE
import           Data.List.Utility                             (duplicates)
import           Data.Map                                      (Map, intersectionWith, keys)
import qualified Data.Map                                      as Map
import           Data.Maybe                                    (catMaybes, fromMaybe)
import           Data.NodeLabel
import           Data.Normalization.Character
import           Data.Normalization.Metadata
import           Data.Normalization.Topology
import           Data.Semigroup                                (sconcat, (<>))
import           Data.Semigroup.Foldable
import           Data.Set                                      (Set, (\\))
import qualified Data.Set                                      as Set
import           Data.String
import           Data.TCM                                      (TCM, TCMStructure (..))
import qualified Data.TCM                                      as TCM
import           Data.Text.Short                               (ShortText, toString)
import qualified Data.Text.Short                               as TS
--import           Data.Vector                                   (Vector)
import           Data.Vector.NonEmpty                          (Vector)
import           PCG.Command.Read.Unification.UnificationError
import           Prelude                                       hiding (lookup, zipWith)


data FracturedParseResult
   = FPR
   { parsedChars   :: NormalizedCharacters
   , parsedMetas   :: Maybe (Vector NormalizedMetadata)
   , parsedForests :: NormalizedForestSet
   , relatedTcm    :: Maybe (TCM, TCMStructure)
   , sourceFile    :: FilePath
   }


data ParseData
  = ParseData
  { dataSequences :: [FracturedParseResult]
  , taxaSet       :: Set Identifier 
  , allForests    :: [FracturedParseResult]
  , forestTaxa    :: [([NonEmpty Identifier], FracturedParseResult)]
  }


type FileSource = ShortText


instance Show FracturedParseResult where

    show fpr = unlines
        [ "FPR"
        , "  { parsedChars   = " <> show (parsedChars fpr)
        , "  , parsedMetas   = " <> show (parsedMetas fpr)
        , "  , parsedForests = " <> if null (parsedForests fpr) then "Nothing" else "Just <trees>"
        , "  , relatedTcm    = " <> show (relatedTcm  fpr)
        , "  , sourceFile    = " <> show (sourceFile  fpr)
        , "  }"
        ]


masterUnify
  :: Foldable1 f
  => f FracturedParseResult
  -> Either UnificationError (Either TopologicalResult CharacterResult)
masterUnify = rectifyResults2


parmap' :: Traversable t => (a -> b) -> t a -> t b
parmap' = parmap rpar


-- |
-- Unify disparate parsed results into a single phylogenetic solution.
rectifyResults2
  :: Foldable1 f
  => f FracturedParseResult
  -> Either UnificationError (Either TopologicalResult CharacterResult)
rectifyResults2 fprs =
    case errors of
      []   -> fmap (fmap reifiedSolution) dagForest
      x:xs -> Left . sconcat $ x:|xs
  where

    parseData@ParseData{..} = gatherParseData fprs
    errors                  = getUnificationErrors parseData

    -- Combine disparate sequences from many sources into single
    -- metadata & character sequence.
    -- TODO: Change type (Maybe a, b) to Maybe (a, b)
    seqs :: (Maybe UnifiedMetadataSequence, Map ShortText UnifiedCharacterSequence)
    seqs@(metaSeq,charSeqs) = joinSequences dataSequences

    -- Collect the parsed forests to be merged
    suppliedForests :: [PhylogeneticForest NormalizedTree]
    suppliedForests = foldMap toList . catMaybes $ parsedForests `parmap'` allForests

    -- Convert topological forests to DAGs (using reference indexing from #7 results)
    dagForest =
        case (hasData suppliedForests, hasData charSeqs, metaSeq) of
          -- Throw a unification error here
          (False, False, _        ) -> Left . UnificationError . pure . VacuousInput $ sourceFile <$> toNonEmpty fprs
          (_    , True , Nothing  ) -> Left . UnificationError . pure . VacuousInput $ sourceFile <$> toNonEmpty fprs

          -- Build a default forest of singleton components
          (False, True , Just meta) -> Right . Right . PhylogeneticSolution . pure
                          . foldMap1 (singletonComponent meta) . NE.fromList $ toKeyedList charSeqs

          -- Build a forest of with Units () as character type parameter
          (True , False, _        ) -> Right . Left  . PhylogeneticSolution $ NE.fromList suppliedForests

          -- Build a forest with the corresponding character data on the nodes
          (True , True , Just meta) -> Right . Right . PhylogeneticSolution $ matchToChars meta charSeqs <$> NE.fromList suppliedForests
      where
        hasData :: Foldable f => f a -> Bool
        hasData = not . null
        
        singletonComponent meta (label, datum) = PhylogeneticForest . pure . PDAG meta $ DAG.fromList
            [ (        mempty, PNode (nodeLabel "Trivial Root") (defaultCharacterSequenceDatum charSeqs), IM.singleton 1 mempty)
            , (IS.singleton 0, PNode (nodeLabel label         )                         datum, mempty               )
            ]


defaultCharacterSequenceDatum 
  :: (HasBlocks 
       s 
       t
       (f (CharacterBlock b1 b2 b3 b4 b5 b6)) 
       (f (CharacterBlock
            (Maybe a1)
            (Maybe a2)
            (Maybe a3)
            (Maybe a4)
            (Maybe a5)
            (Maybe a6)
          )
        )
      , Functor f
      , Foldable c
      ) => c s -> t

defaultCharacterSequenceDatum charSeqs = over blockSequence (fmap blockTransform) . head $ toList charSeqs
  where
    blockTransform = hexmap f f f f f f
    f = const Nothing


matchToChars
  :: UnifiedMetadataSequence
  -> Map ShortText UnifiedCharacterSequence
  -> PhylogeneticForest NormalizedTree
  -> PhylogeneticForest UnReifiedCharacterDAG
matchToChars meta charMapping = fmap (PDAG meta . fmap f)
  where
    f label = PNode nodelabel_ $ fromMaybe (defaultCharacterSequenceDatum charMapping) charLabelMay
      where
        labelShort :: Maybe ShortText
        labelShort   = coerce label

        nodelabel_ :: NodeLabel
        nodelabel_ = fromMaybe def label

        charLabelMay :: Maybe UnifiedCharacterSequence
        charLabelMay = labelShort >>= (`lookup` charMapping)


type PartiallyUnififedCharacterSequence  a = (NormalizedCharacter, NormalizedMetadata, FileSource, a, CharacterName)


type PartiallyUnififedCharacterSequences a = Map Identifier (NonEmpty (PartiallyUnififedCharacterSequence a))


-- |
-- Joins the sequences of a fractured parse result. This requires several
-- sequential steps. Each fractured parse result will be placed into a seperate
-- character block by default. We collapse and merge these seperate parse results
-- in the last step to ensure that the input data is placed into it's propper
-- character block.
--
-- * First we collect all character and file names, and atomically generate
--   well-typed CharcterName list. We must not collapse the structure.
--
-- * Next We disambiguate or default which TCM should be used for each input
--   character. We assume that the fractured parse result alphabet that is
--   supplied is of equal length to the selected TCM dimension. This assumption
--   should be safe, though it is the burden of the caller to ensure this input
--   invariant.
--
-- * Afterwards we attempt to reduce the alphabet and TCMs by looking for
--   symbols present in the alphabet that do not appear in any input character.
--   Extyraneous symbols are removed from the character alphabet and the
--   corresponding TCM is reduced in size.
--
-- * Lastly we collapse the many parse results into a single map of charcter
--   blocks wrapped together as a charcter sequence. This will properly add
--   missing character values to taxa provided in other files.
joinSequences :: Foldable t => t FracturedParseResult -> (Maybe UnifiedMetadataSequence, Map ShortText UnifiedCharacterSequence)
joinSequences = collapseAndMerge . performMetadataTransformations . deriveCorrectTCMs . deriveCharacterNames
  where
    deriveCorrectTCMs
      :: Functor f
      => f (Map Identifier (NonEmpty (NormalizedCharacter, NormalizedMetadata, FileSource, Maybe (TCM, TCMStructure), CharacterName)))
      -> f (Map Identifier (NonEmpty (NormalizedCharacter, NormalizedMetadata, FileSource, TCM, TCMStructure, CharacterName)))
    deriveCorrectTCMs = fmap (fmap (fmap selectTCM))
      where
        selectTCM (charMay, charMetadata, charSource, tcmMay, charName) = (charMay, charMetadata, tcmSource, selectedTCM, selectedStructure, charName)
          where
            (selectedTCM, selectedStructure, tcmSource)
              = case (tcmMay, parsedTCM charMetadata) of
                  (Just (t,s), _)    -> (t, s, charSource)
                  (_, Just (t,s))    -> (t, s, charSource)
                  (Nothing, Nothing) -> defaultTCMData


            specifiedAlphabet = alphabet charMetadata
            defaultTCMData    = (TCM.generate (length specifiedAlphabet) defaultCost, NonAdditive, "N/A")
              where
                defaultCost  :: (Word, Word) -> Word
                defaultCost (i,j)
                  | i == j    = 0
                  | otherwise = 1

    performMetadataTransformations
      :: Functor f
      => f (Map Identifier (NonEmpty (NormalizedCharacter, NormalizedMetadata, FileSource, TCM, TCMStructure, CharacterName)))
      -> f (Map Identifier (NonEmpty (NormalizedCharacter, NormalizedMetadata, FileSource, Word -> Word -> Word, TCMStructure, CharacterName)))
    performMetadataTransformations = fmap reduceFileBlock
      where
        reduceFileBlock mapping = fmap (zipWith updateMetadataInformation updatedMetadataTokens) mapping
          where
            updatedMetadataTokens :: NonEmpty (Alphabet ShortText, Word -> Word -> Word)
            updatedMetadataTokens = fmap generateMetadataToken . NE.fromList . transpose . fmap toList $ toList mapping
             where
               generateMetadataToken                []  = error "Should never happen in reduceAlphabets.reduceFileBlock.observedSymbolSets.generateObservedSymbolSetForCharacter" -- mempty
               generateMetadataToken (_x@(_,m,_,tcm,structure,_):_xs) = (reducedAlphabet, reducedTCM)
                 where
                   suppliedAlphabet = alphabet m
                   reducedAlphabet  = suppliedAlphabet
                   reducedTCM =
                       case structure of
                         NonAdditive -> nonAdditiveDistanceFunction
                         Additive    -> additiveDistanceFunction
                         _           -> let !tcm' = tcm -- TCM.reduceTcm missingSymbolIndicies tcm
                                        in (\i j -> toEnum . fromEnum $ tcm' TCM.! (i,j))

        updateMetadataInformation
          :: (Alphabet ShortText, Word -> Word -> Word)
          -> (NormalizedCharacter, NormalizedMetadata, FileSource, TCM, TCMStructure, CharacterName)
          -> (NormalizedCharacter, NormalizedMetadata, FileSource, Word -> Word -> Word, TCMStructure, CharacterName)
        updateMetadataInformation (reducedAlphabet, symbolDistance) (charMay, charMetadata, tcmSourceFile, _, structure, charName) =
            ( charMay
            , charMetadata { alphabet = reducedAlphabet }
            , tcmSourceFile
            , symbolDistance
            , structure
            , charName
            )

    collapseAndMerge
      :: Foldable f
      => f (Map Identifier (NonEmpty (NormalizedCharacter, NormalizedMetadata, FileSource, Word -> Word -> Word, TCMStructure, CharacterName)))
      -> (Maybe UnifiedMetadataSequence, Map ShortText UnifiedCharacterSequence)
    collapseAndMerge = (fmap MD.fromNonEmpty *** fmap CS.fromNonEmpty) . fst . foldl' f ((mempty, mempty), [])
      where
        f :: ((Maybe (NonEmpty UnifiedMetadataBlock), Map Identifier (NonEmpty UnifiedCharacterBlock)), [UnifiedCharacterBlock])
          -> Map ShortText (NonEmpty (NormalizedCharacter, NormalizedMetadata, ShortText, Word -> Word -> Word, TCMStructure, CharacterName))
          -> ((Maybe (NonEmpty UnifiedMetadataBlock), Map Identifier (NonEmpty UnifiedCharacterBlock)), [UnifiedCharacterBlock])
        f ((prevMeta, prevMapping), prevPad) currTreeChars = ((nextMeta, nextMapping), nextPad)
          where
            nextMapping   = inOnlyPrev <> inBoth <> inOnlyCurr
            nextPad       = prevPad <> toList currPad -- generate (length nextMetaData) (const Nothing)

            currPad       = fmap toMissingCharacters . head $ toList currMapping
            currMapping   = pure . encodeToCharacterBlock <$> currTreeChars

            nextMeta =
                case toList currTreeChars of
                  []  -> prevMeta
                  x:_ -> prevMeta <> Just (pure (buildMetadataBlock x))

            inBoth        = intersectionWith (<>) prevMapping currMapping-- oldTreeChars nextTreeChars
            inOnlyCurr    =  prepend prevPad  <$> getUnique currMapping prevMapping
            inOnlyPrev    = (<>      currPad) <$> getUnique prevMapping currMapping

            getUnique x y = x `Map.restrictKeys` (lhs `Set.difference` rhs)
              where
                lhs = Set.fromList $ keys x
                rhs = Set.fromList $ keys y


            -- Necessary for mixing [] with NonEmpty
            prepend :: [a] -> NonEmpty a -> NonEmpty a
            prepend list ne =
              case list of
                []   -> ne
                x:xs -> x :| (xs <> toList ne)


-- |
-- We do this to correctly construct the CharacterNames.
--
-- We must be careful here, prealigned data requires unique names for each character "column" in the sequence.
deriveCharacterNames
  :: Foldable t
  => t FracturedParseResult
  -> [PartiallyUnififedCharacterSequences (Maybe (TCM, TCMStructure))]
deriveCharacterNames xs = reverse . snd $ foldl' g (charNames, []) xs
      where
        g :: ([CharacterName], [PartiallyUnififedCharacterSequences (Maybe (TCM, TCMStructure))])
          -> FracturedParseResult
          -> ([CharacterName], [PartiallyUnififedCharacterSequences (Maybe (TCM, TCMStructure))])
        g (properNames, ys) fpr = (properNames', newMap:ys)
          where
            localMetadata = parsedMetas fpr

            -- We remove the CharacterName values that we assigned to the characters in this map.
            properNames'  = drop (length localMetadata) properNames

            -- This call to NE.fromList is """safe""", we checked that there were no empty characters.
            newMap = charMapToSplitValues <$> parsedChars fpr

            charMapToSplitValues
              :: Vector NormalizedCharacter
              -> NonEmpty (NormalizedCharacter, NormalizedMetadata, FileSource, Maybe (TCM, TCMStructure), CharacterName)
            charMapToSplitValues x = NE.fromList $ zip5
                (toList x)
                (foldMap toList localMetadata)
                (repeat (fromString . sourceFile $ fpr))
                (repeat (relatedTcm fpr))
                properNames   

        charNames :: [CharacterName]
        charNames = makeCharacterNames . concatMap nameTransform $ toList xs
          where
            nameTransform x = fmap (const (sourceFile x) &&& correctName . characterName) . foldMap toList $ parsedMetas x
            correctName txt
              | TS.null txt = Nothing
              | otherwise   = Just txt


fromTreeOnlyFile :: FracturedParseResult -> Bool
fromTreeOnlyFile fpr = null chars || all null chars
  where
    chars = parsedChars fpr


terminalNames2 :: ReferenceDAG a b (Maybe NodeLabel) -> [Identifier]
terminalNames2 dag = coerce $ catMaybes $ (`nodeDatum` dag) <$> leaves dag


gatherForestsTerminalNames :: FracturedParseResult -> ([NonEmpty Identifier], FracturedParseResult)
gatherForestsTerminalNames fpr = (identifiers, fpr)
  where
    identifiers :: [NonEmpty Identifier]
    identifiers =
        case parsedForests fpr of
          Nothing      -> []
          Just (e:|es) -> catMaybes $ f <$> (e:es)
      where
        f forest =
          case foldMap terminalNames2 forest of
             []   -> Nothing
             x:xs -> Just (x :| xs)



gatherParseData
  :: Foldable1 f
  => f FracturedParseResult
  -> ParseData
gatherParseData fprs = ParseData{..}
  where
       -- Gather data file contents
    dataSequences   = filter (not . fromTreeOnlyFile) $ toList fprs
    -- Union the taxa names together into total terminal set
    taxaSet         = mconcat $ (Set.fromList . keys . parsedChars) `parmap'` dataSequences
    -- Gather forest file data
    allForests      = filter (not . null . parsedForests) $ toList fprs
    -- Gather the taxa names for each forest from terminal nodes
    forestTaxa      = gatherForestsTerminalNames `parmap'` allForests


getUnificationErrors :: ParseData -> [UnificationError]
getUnificationErrors ParseData{..} = catMaybes [duplicateError, extraError, missingError]
  where
    --Assert that each terminal node name is unique in each forest
    duplicateNames :: [([[Identifier]], FracturedParseResult)]
    duplicateNames = filter (not . all null . fst) $ first (fmap duplicates) `parmap'` forestTaxa
    -- Assert that each forest's terminal node set is exactly the same
    -- as the taxa set from "data files"
    extraNames   :: [([Set Identifier], FracturedParseResult)]
    extraNames      = filter (not . all null . fst) $ first (fmap ((\\ taxaSet) . Set.fromList . toList)) `parmap'` forestTaxa

    missingNames :: [([Set Identifier], FracturedParseResult)]
    missingNames    = filter (not . all null . fst) $ first (fmap ((taxaSet \\) . Set.fromList . toList)) `parmap'` forestTaxa

 -- Error collecting
    duplicateError = constructErrorMaybe ForestDuplicateTaxa duplicateNames
    extraError     = constructErrorMaybe ForestExtraTaxa     extraNames
    missingError   = constructErrorMaybe ForestMissingTaxa   missingNames

    constructErrorMaybe :: Foldable f
                        => (NonEmpty Identifier -> FilePath -> UnificationErrorMessage)
                        -> [([f Identifier], FracturedParseResult)]
                        -> Maybe UnificationError
    constructErrorMaybe f xs =
        case catMaybes $ colateErrors f <$> expandForestErrors xs of
          []   -> Nothing
          y:ys -> Just . fold1 $ y:|ys

    colateErrors :: (Foldable t, Foldable t')
                 => (NonEmpty Identifier -> FilePath -> UnificationErrorMessage)
                 -> t (t' Identifier, FracturedParseResult)
                 -> Maybe UnificationError
    colateErrors f xs =
      case toList xs of
        [] -> Nothing
        ys -> Just . UnificationError . NE.fromList $ transformFPR <$> ys
      where
        transformFPR (x,y) = f (NE.fromList $ toList x) $ sourceFile y

    expandForestErrors
      :: [([t a], FracturedParseResult)]
      -> [[(t a, FracturedParseResult)]]
    expandForestErrors = fmap f
      where
        f (ys, fpr) = (id &&& const fpr) <$> ys


additiveDistanceFunction :: Word -> Word -> Word
additiveDistanceFunction i j = max i j - min i j

nonAdditiveDistanceFunction :: Word -> Word -> Word
nonAdditiveDistanceFunction i j
  | i == j    = 0
  | otherwise = 1


buildMetadataBlock
  :: Foldable1 t
  => t (NormalizedCharacter
       , NormalizedMetadata
       , ShortText
       , Word -> Word -> Word
       , TCMStructure
       , CharacterName
       )
  -> UnifiedMetadataBlock
buildMetadataBlock = foldMap1 encodeToSingletonMetadata
  where
    encodeToSingletonMetadata
      :: (NormalizedCharacter
         , NormalizedMetadata
         , ShortText
         , Word -> Word -> Word
         , TCMStructure
         , CharacterName
         )
         -> MetadataBlock ()
    encodeToSingletonMetadata
        (charMay, charMeta, tcmSource,  scm, structure, charName) =
          case charMay of
            NormalizedContinuousCharacter {} ->
                MD.continuousToMetadataBlock
              $ continuousMetadata charName charWeight
            NormalizedDiscreteCharacter   {} ->
                MD.discreteToMetadataBlock structure
              $ discreteMetadataWithTCM charName charWeight specifiedAlphabet tcmSource scm
            NormalizedDynamicCharacter    {} ->
                MD.dynamicToMetadataBlock
              $ dynamicMetadataWithTCM charName charWeight specifiedAlphabet tcmSource scm
     where
      charWeight        = weight   charMeta
      specifiedAlphabet = fmap toString . alphabet  $ charMeta

encodeToCharacterBlock
  :: Foldable1 t
  => t (NormalizedCharacter
       , NormalizedMetadata
       , ShortText
       , Word -> Word -> Word
       , TCMStructure
       , CharacterName
       )
  -> UnifiedCharacterBlock
encodeToCharacterBlock = finalizeCharacterBlock . foldMap1 encodeBinToSingletonCharacterBlock
  where
    encodeBinToSingletonCharacterBlock
      :: (NormalizedCharacter
         , NormalizedMetadata
         , ShortText
         , Word -> Word -> Word
         , TCMStructure
         , CharacterName
         )
      -> PartialCharacterBlock
           UnifiedContinuousCharacter
           UnifiedDiscreteCharacter
           UnifiedDiscreteCharacter
           UnifiedDiscreteCharacter
           UnifiedDiscreteCharacter
           UnifiedDynamicCharacter
    encodeBinToSingletonCharacterBlock
        (charMay, charMeta, _tcmSource, _scm, structure, _charName) =
          case charMay of
            NormalizedContinuousCharacter continuousMay
              -> continuousSingleton . Just . continuousDecorationInitial $ toContinuousCharacter continuousMay
            NormalizedDiscreteCharacter   discreteMay
              ->   discreteSingleton structure . Just $ toDiscreteCharacterDecoration staticTransform discreteMay
            NormalizedDynamicCharacter     dynamicMay
              ->    dynamicSingleton . Just $  toDynamicCharacterDecoration dynamicTransform dynamicMay
      where
        alphabetLength    = toEnum $ length specifiedAlphabet
        specifiedAlphabet = alphabet charMeta
        missingCharValue  = NE.fromList $ toList specifiedAlphabet
        dynamicTransform  = maybe (Missing alphabetLength) (encodeStream specifiedAlphabet)
        staticTransform   = encodeElement specifiedAlphabet . fromMaybe missingCharValue
