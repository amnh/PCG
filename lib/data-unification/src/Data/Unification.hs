----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unification
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Containing the command for unifying all input types:
--  - Character
--  - Metadata
--  - Topological
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Unification
  ( PartialInputData(..)
  , UnificationError()
  , unifyPartialInputs
  ) where

import           Bio.Character
import           Bio.Character.Decoration.Continuous hiding (characterName)
import           Bio.Character.Decoration.Discrete   hiding (characterName)
import           Bio.Character.Decoration.Dynamic    hiding (characterName)
import           Bio.Character.Encodable
import           Bio.Graph
import           Bio.Graph.Node
import qualified Bio.Graph.ReferenceDAG              as DAG
import           Bio.Metadata.CharacterName          hiding (sourceFile)
import           Bio.Metadata.Continuous             (continuousMetadata)
import           Bio.Metadata.DiscreteWithTCM        (discreteMetadataFromTCM)
import           Bio.Metadata.Dynamic                (dynamicMetadataFromTCM)
import           Bio.Sequence                        hiding (hexmap)
import           Bio.Sequence.Block
import qualified Bio.Sequence.Character              as CS
import qualified Bio.Sequence.Metadata               as MD
import           Control.Arrow                       ((&&&), (***))
import           Control.DeepSeq
import           Control.Lens                        (over)
import           Control.Monad.State.Strict
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Coerce                         (coerce)
import           Data.Default
import           Data.FileSource
import           Data.Foldable
import           Data.Functor.Identity               (runIdentity)
import qualified Data.IntMap                         as IM
import qualified Data.IntSet                         as IS
import           Data.Key
import           Data.List                           (zip5)
import           Data.List.NonEmpty                  (NonEmpty (..))
import qualified Data.List.NonEmpty                  as NE
import           Data.List.Utility                   (catMaybes1)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import qualified Data.Map.Merge.Strict               as Map
import           Data.Maybe                          (fromMaybe)
import           Data.NodeLabel
import           Data.Normalization.Character
import           Data.Normalization.Metadata
import           Data.Normalization.Topology
import           Data.Semigroup.Foldable
import           Data.TCM                            (TCM, TCMStructure (..))
import qualified Data.TCM                            as TCM
import           Data.Text.Short                     (ShortText, toString)
import qualified Data.Text.Short                     as TS
import           Data.Unification.Error
import           Data.Unification.InputData
import           Data.Validation
import           Prelude                             hiding (lookup)


--type FileSource = ShortText


type PartiallyUnififedCharacterSequence  a = (CharacterName, NormalizedCharacter, NormalizedMetadata, FileSource, a)


type PartiallyUnififedCharacterSequences a = Map Identifier (NonEmpty (PartiallyUnififedCharacterSequence a))


-- |
-- Unify disparate parsed results into a single phylogenetic solution.
unifyPartialInputs
  :: Foldable1 f
  => f PartialInputData
  -> Validation UnificationError (Either TopologicalResult CharacterResult)
unifyPartialInputs pids = collectPartialInputs pids `bindValidation` performUnification inputFileSources
  where
    inputFileSources = sourceFile <$> toNonEmpty pids


performUnification :: NonEmpty FileSource -> InputData -> Validation UnificationError (Either TopologicalResult CharacterResult)
performUnification inputPaths InputData{..} = fmap reifiedSolution <$> dagForest
  where
    -- Collect the parsed forests to be merged
    suppliedForests :: Maybe (NonEmpty (PhylogeneticForest NormalizedTree))
    suppliedForests = (fmap fold1 . catMaybes1 . (parsedForests `pmap`)) =<< allForests

    -- Convert topological forests to DAGs
    dagForest =
        case (suppliedForests, joinSequences <$> dataSequences) of
          -- Throw a unification error here
          (Nothing, Nothing) -> Failure $ vacuousInputFiles inputPaths

          -- Build a forest of with Units () as character type parameter
          (Just someForests, Nothing) -> Success . Left  . PhylogeneticSolution $ someForests

          -- Build a default forest of singleton components
          (Nothing, Just v@(charSeqs, _))
            -> Success . Right . PhylogeneticSolution . pure
             . PhylogeneticForest . NE.fromList
             . foldr ((:) . (singletonComponent v)) mempty $ toKeyedList charSeqs
          -- Build a forest with the corresponding character data on the nodes
          (Just someForests, Just (charSeqs, meta)) -> Success . Right . PhylogeneticSolution $ matchToChars meta charSeqs <$> someForests
      where
        trivialRootName = nodeLabel "Trivial Root"
        trivialRootIndex cs
          = DAG.IndexData
              (PNode trivialRootName (defaultCharacterSequenceDatum cs))
              mempty
              (IM.singleton 1 mempty)

        leafNodeIndex l d
          = DAG.IndexData
              (PNode (nodeLabel l) d)
              (IS.singleton 0)
              mempty

        singletonComponent (charSeqs, meta) (label, datum) = PDAG meta $
          DAG.trivialRefDAG (trivialRootIndex charSeqs) (leafNodeIndex label datum)



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


-- |
-- Joins the sequences of a fractured parse result. This requires several
-- sequential steps. Each fractured parse result will be placed into a separate
-- character block by default. We collapse and merge these separate parse results
-- in the last step to ensure that the input data is placed into it's proper
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
-- * Lastly we collapse the many parse results into a single map of charcter
--   blocks wrapped together as a charcter sequence. This will properly add
--   missing character values to taxa provided in other files.
joinSequences
  :: ( Foldable1 t
     , Traversable t
     )
  => t PartialInputData
  -> (Map ShortText UnifiedCharacterSequence, UnifiedMetadataSequence)
joinSequences = collapseAndMerge . fmap deriveCorrectTCMs . deriveCharacterNames


-- |
-- We do this to correctly construct the CharacterNames.
--
-- We must be careful here, prealigned data requires unique names for each character "column" in the sequence.
deriveCharacterNames
  :: Traversable t => t PartialInputData -> t (PartiallyUnififedCharacterSequences (Maybe (TCM, TCMStructure)))
deriveCharacterNames = inlineName . assignCharacterNames expandIndividualCharacterDatum
  where
    expandIndividualCharacterDatum
      :: PartialInputData
      -> ( FileSource
         , Map Identifier
             (NonEmpty
                ( Maybe ShortText
                , (NormalizedCharacter, NormalizedMetadata, FileSource, Maybe (TCM, TCMStructure))
                )
             )
         )
    expandIndividualCharacterDatum pid = (inputFileSource, expandSeqence <$> parsedChars pid)
      where
        localMetadata   = foldMap toList $ parsedMetas pid
        inputFileSource = force          $ sourceFile  pid
        inputTCM        = force          $ relatedTcm  pid

        expandSeqence v = fmap pullOutName . NE.fromList $ zip5 a b c d e
          where
            a = correctName . characterName <$> localMetadata
            b = toList v
            c = localMetadata
            d = repeat inputFileSource
            e = repeat inputTCM

    inlineName = fmap (fmap (fmap pushInName))

    pullOutName (a,b,c,d,e)   = (a,(b,c,d,e))

    pushInName  (a,(b,c,d,e)) = (a,b,c,d,e)

    correctName txt
      | TS.null txt = Nothing
      | otherwise   = Just txt


-- |
-- We can derive the TCM for a given character from one of three sources:
--
--   1. It can be specified via a "tcm:<path>" annotation in the PCG script
--
--   2. It can be defined, in some file formats, in the same file as the
--      character data
--
--   3. It can be generated to a default value if neither of the above were
--      present.
--
-- The default metric is the discrete metric (non-additive). I believe that due
-- to the structural analysis of the character metrics, we will never use the
-- default generated TCM below and instead only note that it is the discrete
-- metric and compute the values as needed.
deriveCorrectTCMs
  :: PartiallyUnififedCharacterSequences (Maybe (TCM, TCMStructure))
  -> PartiallyUnififedCharacterSequences (TCM, TCMStructure)
deriveCorrectTCMs = fmap (fmap selectTCM)
  where
    selectTCM (charName, charMay, charMetadata, charSource, tcmMay) = (charName, charMay, charMetadata, tcmSource, (selectedTCM, selectedStructure))
      where
        specifiedAlphabet = alphabet charMetadata

        (selectedTCM, selectedStructure, tcmSource) =
            case (tcmMay, parsedTCM charMetadata) of
              (Just (t,s), _)    -> (t, s, charSource)
              (_, Just (t,s))    -> (t, s, charSource)
              (Nothing, Nothing) -> defaultTCMData

        defaultTCMData = (TCM.generate (length specifiedAlphabet) defaultCost, NonAdditive, "No source specified, default TCM generated")
          where
            defaultCost  :: (Word, Word) -> Word
            defaultCost (i,j)
              | i == j    = 0
              | otherwise = 1


collapseAndMerge
  :: Foldable1 f
  => f (PartiallyUnififedCharacterSequences (TCM, TCMStructure))
  -> (Map ShortText UnifiedCharacterSequence, UnifiedMetadataSequence)
collapseAndMerge xs = extractResult $ foldlM sequenceMerge initialMap ms
  where
    -- Get the first map to be used in initializing the fold.
    (fstMap:|ms) = toNonEmpty xs

    -- Unify the characters in the first map as the initial element for the fold.
    initialMap    :: Map ShortText (NonEmpty UnifiedCharacterBlock)
    initialMap    = pure . encodeToCharacterBlock <$> fstMap

    -- Lookup a partial character sequence from the new map to be merged, WLOG.
    -- All partial character sequences in the new map are the same length.
    -- Use the partial character sequences to generate an initial sequences of
    -- metadata block and missing character block to be used as the initial state
    -- for the fold.
    initialState  :: NonEmpty (UnifiedMetadataBlock, UnifiedCharacterBlock)
    initialState  = let (_,v) = Map.findMin fstMap
                    in  pure $ (buildMetadataBlock &&& buildMissingBlock) v

    -- Run the stateful computation and extract the resulting tree from the
    -- computation value and the metadata sequence from the computation state
    extractResult
      :: State (NonEmpty (UnifiedMetadataBlock, UnifiedCharacterBlock))
                           (Map ShortText (NonEmpty UnifiedCharacterBlock))
      -> ( Map ShortText
                (CharacterSequence
                   UnifiedContinuousCharacter
                   UnifiedDiscreteCharacter
                   UnifiedDiscreteCharacter
                   UnifiedDiscreteCharacter
                   UnifiedDiscreteCharacter
                   UnifiedDynamicCharacter)
         , MetadataSequence ()
         )
    extractResult st =
      let
        res = st `runState` initialState
      in
        (fmap CS.fromNonEmpty *** MD.fromNonEmpty . fmap fst) $ res

    sequenceMerge :: Map ShortText (NonEmpty UnifiedCharacterBlock)
                  -> PartiallyUnififedCharacterSequences (TCM, TCMStructure)
                  -> State (NonEmpty (UnifiedMetadataBlock, UnifiedCharacterBlock))
                           (Map ShortText (NonEmpty UnifiedCharacterBlock))
    sequenceMerge accumCharMap partialMap =
        -- Lookup a character sequence from the new map to be merged, WLOG.
        -- All character sequences in the new map are the same length.
        case Map.lookupMin partialMap of
          -- If the map was empty, do no work
          -- This case should never occur in practice, but handling it is trivial.
          Nothing    -> pure accumCharMap
          -- With an characeter sequence selected WLOG,
          -- we proceed to merge the new map with the accumulated map
          -- and update the state with a long pad and longer metadata sequence
          Just (_,v) -> do
              -- Convert all the characters, into a single character block
              let someCharMap = pure . encodeToCharacterBlock <$> partialMap
              -- Convert a character block WLOG, to a new metadata block and a
              -- block of missing character data based on the character data.
              let b@(_, !missingBlock) = (buildMetadataBlock &&& buildMissingBlock) v
              -- Get the missing character blocks from the state sequence
              !prefixOfMissingChars <- fmap snd <$> get
              -- Update the state by appending the new missing character data block
              -- to the growing pad of missing character blocks used as a prefix for
              -- newly encountered taxa.
              -- Additionally, append the newly constructed metadata block to the
              -- metadata sequence being build in the state's context.
              modify (<> pure b)
              -- Lastly, we merge the maps
              pure . force . runIdentity $ Map.mergeA
                  (oldTaxaMissing (pure missingBlock))
                  (newTaxaPresent prefixOfMissingChars)
                  twoTaxaMatched
                  accumCharMap
                  someCharMap
      where
        oldTaxaMissing newPad = Map.traverseMissing $ \_ x   -> pure $      x <> newPad
        newTaxaPresent oldPad = Map.traverseMissing $ \_   y -> pure $ oldPad <> y
        twoTaxaMatched        = Map.zipWithAMatched $ \_ x y -> pure $      x <> y


buildMissingBlock
  :: Foldable1 t
  => t (PartiallyUnififedCharacterSequence (TCM, TCMStructure))
  -> UnifiedCharacterBlock
buildMissingBlock = toMissingCharacters . encodeToCharacterBlock


buildMetadataBlock
  :: Foldable1 t
  => t (PartiallyUnififedCharacterSequence (TCM, TCMStructure))
  -> UnifiedMetadataBlock
buildMetadataBlock = foldMap1 encodeToSingletonMetadata
  where
    encodeToSingletonMetadata
      :: PartiallyUnififedCharacterSequence (TCM, TCMStructure)
      -> MetadataBlock ()
    encodeToSingletonMetadata
        (charName,  charMay, charMeta, tcmSource, (tcm, structure)) =
          case charMay of
            NormalizedContinuousCharacter {} ->
                MD.continuousToMetadataBlock
              $ continuousMetadata charName charWeight
            NormalizedDiscreteCharacter   {} ->
                MD.discreteToMetadataBlock structure
              $ discreteMetadataFromTCM charName charWeight specifiedAlphabet tcmSource tcm
            NormalizedDynamicCharacter    {} ->
                MD.dynamicToMetadataBlock
              $  dynamicMetadataFromTCM charName charWeight specifiedAlphabet tcmSource tcm
     where
      charWeight        = weight charMeta
      specifiedAlphabet = fmap toString . alphabet $ charMeta


encodeToCharacterBlock
  :: Foldable1 t
  => t (PartiallyUnififedCharacterSequence (TCM, TCMStructure))
  -> UnifiedCharacterBlock
encodeToCharacterBlock = finalizeCharacterBlock . foldMap1 encodeBinToSingletonCharacterBlock
  where
    encodeBinToSingletonCharacterBlock
      :: PartiallyUnififedCharacterSequence (TCM, TCMStructure)
      -> PartialCharacterBlock
           UnifiedContinuousCharacter
           UnifiedDiscreteCharacter
           UnifiedDiscreteCharacter
           UnifiedDiscreteCharacter
           UnifiedDiscreteCharacter
           UnifiedDynamicCharacter
    encodeBinToSingletonCharacterBlock
        ( _charName, charMay, charMeta, _tcmSource, (_tcm, structure)) =
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


pmap :: Traversable t => (a -> b) -> t a -> t b
pmap = parmap rpar
