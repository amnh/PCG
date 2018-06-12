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

{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module PCG.Command.Read.Unification.Master
  ( FracturedParseResult(..)
  , masterUnify
  ) where

import           Bio.Character
import           Bio.Character.Encodable
import           Bio.Character.Decoration.Continuous hiding (characterName, toContinuousCharacter)
import           Bio.Character.Decoration.Discrete   hiding (characterName)
import           Bio.Character.Decoration.Dynamic    hiding (characterName)
import           Bio.Character.Parsed
import           Bio.Sequence                        hiding (hexmap)
import           Bio.Sequence.Block
import qualified Bio.Sequence.Metadata               as MD
import           Bio.Metadata.Continuous                    (continuousMetadata)
import           Bio.Metadata.Discrete                      (discreteMetadata)
import           Bio.Metadata.DiscreteWithTCM               (discreteMetadataWithTCM)
import           Bio.Metadata.Dynamic                       (dynamicMetadataWithTCM)
import           Bio.Metadata.CharacterName          hiding (sourceFile)
import           Bio.Metadata.Parsed
import           Bio.Graph
import           Bio.Graph.Component
import           Bio.Graph.Forest.Parsed
import           Bio.Graph.Node
import           Bio.Graph.ReferenceDAG
import qualified Bio.Graph.ReferenceDAG     as DAG
import           Control.Arrow                     ((***), (&&&))
import           Control.Applicative               ((<|>))
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Alphabet
import           Data.Bifunctor                    (first)
import           Data.Default
import           Data.Foldable
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import           Data.Key
import           Data.List                         (transpose, zip4)
import           Data.List.NonEmpty                (NonEmpty(..))
import qualified Data.List.NonEmpty         as NE
import           Data.List.Utility                 (duplicates)
import           Data.Map                          (Map, intersectionWith, keys)
import qualified Data.Map                   as Map
import           Data.Maybe                        (catMaybes, fromMaybe, listToMaybe)
import           Data.Semigroup                    ((<>), sconcat)
import           Data.Semigroup.Foldable
import           Data.Set                          (Set, (\\))
import qualified Data.Set                   as Set
import           Data.String
import           Data.TCM                          (TCM, TCMStructure(..))
import qualified Data.TCM                   as TCM
import           Data.MonoTraversable
import           Data.Vector                       (Vector)
import           PCG.Command.Read.Unification.UnificationError
import           Prelude                    hiding (lookup, unzip, zipWith)


data FracturedParseResult
   = FPR
   { parsedChars   :: TaxonCharacters
   , parsedMetas   :: Vector ParsedCharacterMetadata -- Vector StandardMetadata
   , parsedForests :: ParserForestSet
   , relatedTcm    :: Maybe (TCM, TCMStructure)
   , sourceFile    :: FilePath
   }


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


masterUnify :: [FracturedParseResult] -> Either UnificationError (Either TopologicalResult CharacterResult)
masterUnify = rectifyResults2


parmap' :: Traversable t => (a -> b) -> t a -> t b
parmap' = parmap rpar


-- |
-- Unify disparate parsed results into a single phylogenetic solution.
rectifyResults2 :: [FracturedParseResult]
                -> Either UnificationError (Either TopologicalResult CharacterResult)
--rectifyResults2 fprs | trace (show fprs) False = undefined
rectifyResults2 fprs =
    case errors of
      []   -> fmap (fmap reifiedSolution) dagForest --      = undefined -- Right maskedSolution
      x:xs -> Left . sconcat $ x:|xs
  where
    -- Step 1: Gather data file contents
    dataSeqs        = filter (not . fromTreeOnlyFile) fprs
    -- Step 2: Union the taxa names together into total terminal set
    taxaSet         = {- (\x ->  trace ("Taxa Set: " <> show x) x) . -} mconcat $ (Set.fromList . keys . parsedChars) `parmap'` dataSeqs
    -- Step 3: Gather forest file data
    allForests      = {- (\x ->  trace ("Forest Lengths: " <> show (length . parsedTrees <$> x)) x) $ -} filter (not . null . parsedForests) fprs
    -- Step 4: Gather the taxa names for each forest from terminal nodes
    forestTaxa :: [([NonEmpty Identifier], FracturedParseResult)]
    forestTaxa      = {- (\x ->  trace ("Forest Set: " <> show x) x) . -} gatherForestsTerminalNames `parmap'` allForests
    -- Step 5: Assert that each terminal node name is unique in each forest
    duplicateNames :: [([[Identifier]], FracturedParseResult)]
    duplicateNames  = filter (not . all null . fst) $ first (fmap duplicates) `parmap'` forestTaxa
    -- Step 6: Assert that each forest's terminal node set is exactly the same as the taxa set from "data files"
    extraNames   :: [([Set Identifier], FracturedParseResult)]
    extraNames      = filter (not . all null . fst) $ first (fmap ((\\ taxaSet) . Set.fromList . toList)) `parmap'` forestTaxa
    missingNames :: [([Set Identifier], FracturedParseResult)]
    missingNames    = filter (not . all null . fst) $ first (fmap ((taxaSet \\) . Set.fromList . toList)) `parmap'` forestTaxa
    -- Step 7: Combine disparte sequences from many sources  into single metadata & character sequence.
    charSeqs        = joinSequences2 dataSeqs
    -- Step 8: Collect the parsed forests to be merged
    suppliedForests :: [PhylogeneticForest ParserTree]
    suppliedForests = foldMap toList . catMaybes $ parsedForests `parmap'` allForests

    -- Step 9: Convert topological forests to DAGs (using reference indexing from #7 results)
    dagForest       =
        case (null suppliedForests, null charSeqs) of
          -- Throw a unification error here
          (True , True ) -> Left . UnificationError . pure . VacuousInput $ sourceFile <$> NE.fromList fprs
          -- Build a default forest of singleton components
          (True , False) -> Right . Right . PhylogeneticSolution . pure
                          . foldMap1 singletonComponent . NE.fromList $ toKeyedList charSeqs
          -- Build a forest of with Units () as character type parameter
          (False, True ) -> Right . Left  . PhylogeneticSolution $ NE.fromList suppliedForests
          -- BUild a forest with the corresponding character data on the nodes
          (False, False) -> Right . Right . PhylogeneticSolution $ matchToChars charSeqs <$> NE.fromList suppliedForests
      where
        defaultCharacterSequenceDatum = fromBlocks . fmap blockTransform . toBlocks . head $ toList charSeqs
          where
            blockTransform = hexmap f f f f f f
            f = const Nothing

        singletonComponent (label, datum) = PhylogeneticForest . pure . PDAG $ DAG.fromList
            [ (        mempty, PNode (fromString "Trivial Root") defaultCharacterSequenceDatum, IM.singleton 1 mempty)
            , (IS.singleton 0, PNode (fromString label         )                         datum, mempty               )
            ]

        matchToChars :: Map String UnifiedSequence
                     -> PhylogeneticForest ParserTree
                     -> PhylogeneticForest UnReifiedCharacterDAG --CharacterDAG
        matchToChars charMapping = fmap (PDAG . fmap f)
          where
            f label = PNode nodeLabel $ fromMaybe defaultCharacterSequenceDatum charLabelMay
              where
                nodeLabel    =
                    case label of
                      Nothing -> def
                      Just xs -> fromString xs
                charLabelMay = label >>= (`lookup` charMapping)

    -- Error collection
    errors          = catMaybes [duplicateError, extraError, missingError]
    duplicateError  = listToMaybe . catMaybes $ colateErrors ForestDuplicateTaxa <$> expandForestErrors duplicateNames
    extraError      = listToMaybe . catMaybes $ colateErrors ForestExtraTaxa     <$> expandForestErrors extraNames
    missingError    = listToMaybe . catMaybes $ colateErrors ForestMissingTaxa   <$> expandForestErrors missingNames

    colateErrors :: (Foldable t, Foldable t')
                 => (NonEmpty a -> FilePath -> UnificationErrorMessage)
                 -> t (t' a, FracturedParseResult)
                 -> Maybe UnificationError
    colateErrors f xs =
      case toList xs of
        [] -> Nothing
        ys -> Just . UnificationError . NE.fromList $ transformFPR <$> ys
      where
        transformFPR (x,y) = f (NE.fromList $ toList x) $ sourceFile y

    -- [([Set Identifier], FracturedParseResult)]
    expandForestErrors :: [([t TaxaName], FracturedParseResult)] -> [[(t TaxaName, FracturedParseResult)]]
    expandForestErrors = fmap f
      where
        f (ys, fpr) = (\x -> (x, fpr)) <$> ys


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
joinSequences2 :: Foldable t => t FracturedParseResult -> Map String UnifiedSequence
joinSequences2 = collapseAndMerge . performMetadataTransformations . deriveCorrectTCMs . deriveCharacterNames
  where

    -- We do this to correctly construct the CharacterNames.
    deriveCharacterNames
      :: Foldable t
      => t FracturedParseResult
      -> [ Map String (NonEmpty (ParsedCharacter, ParsedCharacterMetadata, Maybe (TCM, TCMStructure), CharacterName)) ]
    deriveCharacterNames xs = reverse . snd $ foldl' g (charNames, []) xs
      where
        g (propperNames, ys) fpr = (drop (length localMetadata) propperNames, newMap:ys)
          where
            localMetadata = parsedMetas fpr
            -- This call to Ne.fromList is safe, we checked that there were no empty characters in Step 1. (not realy though)
            newMap = (\x -> NE.fromList $ zip4 (toList x) (toList localMetadata) (repeat (relatedTcm fpr)) propperNames) <$> parsedChars fpr

        charNames :: [CharacterName]
        charNames = makeCharacterNames . concatMap transform $ toList xs
          where
            transform x = fmap (const (sourceFile x) &&& correctName . characterName) . toList $ parsedMetas x
            correctName [] = Nothing
            correctName ys = Just ys

    deriveCorrectTCMs
      :: Functor f
      => f (Map String (NonEmpty (ParsedCharacter, ParsedCharacterMetadata, Maybe (TCM, TCMStructure), CharacterName)))
      -> f (Map String (NonEmpty (ParsedCharacter, ParsedCharacterMetadata,        TCM, TCMStructure, CharacterName)))
    deriveCorrectTCMs = fmap (fmap (fmap selectTCM))
      where
        selectTCM (charMay, charMetadata, tcmMay, charName) = (charMay, charMetadata, selectedTCM, selectedStructure, charName)
          where
            (selectedTCM, selectedStructure) = fromMaybe defaultTCM $ tcmMay <|> parsedTCM charMetadata
            specifiedAlphabet = alphabet charMetadata
            defaultTCM        = (TCM.generate (length specifiedAlphabet) defaultCost, NonAdditive)
              where
                defaultCost  :: (Word, Word) -> Word
                defaultCost (i,j)
                  | i == j    = 0
                  | otherwise = 1
{-
    reduceAlphabets :: Functor f
                    => f (Map String (NonEmpty (Maybe ParsedChar, ParsedCharacterMetadata, TCM, TCMStructure, CharacterName)))
                    -> f (Map String (NonEmpty (Maybe ParsedChar, ParsedCharacterMetadata, TCM, TCMStructure, CharacterName)))
    reduceAlphabets = fmap reduceFileBlock
      where
        reduceFileBlock mapping = fmap (zipWith removeExtraneousSymbols observedSymbolSets) mapping
          where
            observedSymbolSets :: NonEmpty (Set String, Int, TCM, TCMStructure)
            observedSymbolSets = fmap generateObservedSymbolSetForCharacter . NE.fromList . transpose . fmap toList $ toList mapping
             where
               generateObservedSymbolSetForCharacter input =
                 case input of
                   []             -> error "Should never happen in reduceAlphabets.reduceFileBlock.observedSymbolSets.generateObservedSymbolSetForCharacter" -- mempty
                   x@(_,m,tcm,_):xs ->
                     let diagnosis   = TCM.diagnoseTcm tcm
                         weighting   = TCM.factoredWeight diagnosis
                         tcm'        = TCM.factoredTcm    diagnosis
                         structure   = TCM.tcmStructure   diagnosis
                         seenSymbols =
                             case structure of
                               TCM.Additive -> Set.fromList . toList $ alphabet m
                               _            -> foldMap f $ x:xs

                     in (seenSymbols, weighting, tcm', structure)
                     -- TCM structure won't change with columns removed!


               f (x,_,_,_) = foldMap (foldMap (Set.fromList . toList)) x


        removeExtraneousSymbols :: (Set String, Int, TCM, TCMStructure)
                                -> (Maybe ParsedChar, ParsedCharacterMetadata, TCM, TCMStructure, CharacterName)
                                -> (Maybe ParsedChar, ParsedCharacterMetadata, TCM, TCMStructure, CharacterName)
        removeExtraneousSymbols (observedSymbols, coefficient, tcm, structure) (charMay, charMetadata, _, inputStruct, charName)
          | isDynamic charMetadata      = (charMay, charMetadata' ,        tcm, charName, structure)
          | onull missingSymbolIndicies = (charMay, charMetadata' ,        tcm, charName, structure)
          | otherwise                   = (charMay, charMetadata'', reducedTCM, charName, structure)
          where
            charMetadata'  = charMetadata  { weight   = weight charMetadata * fromIntegral coefficient }
            charMetadata'' = charMetadata' { alphabet = reducedAlphabet }
--            observedSymbols       = observedSymbols' `Set.remove` "?"
            missingSymbolIndicies = foldMapWithKey f suppliedAlphabet
              where
                f k v
                  |    v `notElem` observedSymbols
                    && v /= gapSymbol suppliedAlphabet = IS.singleton k
                  | otherwise = mempty

            suppliedAlphabet      = alphabet charMetadata
            reducedAlphabet       =
                case alphabetStateNames suppliedAlphabet of
                  [] -> fromSymbols               . reduceTokens $      alphabetSymbols suppliedAlphabet
                  xs -> fromSymbolsWithStateNames . reduceTokens $ zip (alphabetSymbols suppliedAlphabet) xs
              where
                reduceTokens = foldMapWithKey (\k v -> if k `oelem` missingSymbolIndicies then [] else [v])
            reducedTCM = TCM.generate reducedDimension genFunction
              where
                reducedDimension = TCM.size tcm - olength missingSymbolIndicies

                genFunction =
                    -- In the case of Additive characters,
                    -- we don't want to preserve the transition cost to non-observed states.
                    --
                    -- In the matrix below, with '2' unobserved, would be naively transformed:
                    --
                    -- 0 1 2 3        0 1 3
                    -- 1 0 1 2  ===>  1 0 2
                    -- 2 1 0 1        3 2 0
                    -- 3 2 1 0
                    --
                    -- We can see that is incorrect as the Additive structure is not preserved
                    -- after the reduction. We handle this case specially.
                    --
                    -- We might need to do this for Ultra-metric also..?
                    case TCM.tcmStructure $ TCM.diagnoseTcm tcm of
                      TCM.Additive -> \(i,j) -> toEnum $ max i j - min i j
                      _            -> f

                f (i,j) = tcm TCM.! (i', j')
                  where
                    i' = i + iOffset
                    j' = j + jOffset
                    xs = otoList missingSymbolIndicies
                    iOffset = length $ filter (<=i) xs
                    jOffset = length $ filter (<=j) xs
-}


    performMetadataTransformations
      :: Functor f
      => f (Map String (NonEmpty (ParsedCharacter, ParsedCharacterMetadata, TCM, TCMStructure, CharacterName)))
      -> f (Map String (NonEmpty (ParsedCharacter, ParsedCharacterMetadata, Word -> Word -> Word, TCMStructure, CharacterName)))
    performMetadataTransformations = fmap reduceFileBlock
      where
        reduceFileBlock mapping = fmap (zipWith updateMetadataInformation updatedMetadataTokens) mapping
          where
            updatedMetadataTokens :: NonEmpty (Alphabet String, Word -> Word -> Word)
            updatedMetadataTokens = fmap generateMetadataToken . NE.fromList . transpose . fmap toList $ toList mapping
             where
{-
               gatherSymbols (x,_,_,_,_) =
                   case x of
                     ParsedContinuousCharacter     _ -> mempty
                     ParsedDiscreteCharacter  static -> foldMap (Set.fromList . toList) static
                     ParsedDynamicCharacter  dynamic -> foldMap (foldMap (Set.fromList . toList)) dynamic
-}

               generateMetadataToken                []  = error "Should never happen in reduceAlphabets.reduceFileBlock.observedSymbolSets.generateObservedSymbolSetForCharacter" -- mempty
               generateMetadataToken (_x@(_,m,tcm,structure,_):_xs) = (reducedAlphabet, reducedTCM)
                 where
--                   diagnosis   = TCM.diagnoseTcm tcm
--                   weighting   = TCM.factoredWeight diagnosis
--                   tcm'        = TCM.factoredTcm    diagnosis
--                   structure   = TCM.tcmStructure   diagnosis
                         -- In the case of Additive characters,
                         -- we don't want to preserve the transition cost to non-observed states.
                         --
                         -- In the matrix below, with '2' unobserved, would be naively transformed:
                         --
                         -- 0 1 2 3        0 1 3
                         -- 1 0 1 2  ===>  1 0 2
                         -- 2 1 0 1        3 2 0
                         -- 3 2 1 0
                         --
                         -- We can see that is incorrect as the Additive structure is not preserved
                         -- after the reduction. We handle this case specially.
                         --
                         -- We might need to do this for Ultra-metric also..?
--                   seenSymbols = foldMap gatherSymbols $ x:xs
{-
                       case structure of
                         Additive -> Set.fromList . toList $ alphabet m
                         _        -> foldMap gatherSymbols $ x:xs
-}

--            observedSymbols       = observedSymbols' `Set.remove` "?"
{-
                   missingSymbolIndicies = foldMapWithKey f suppliedAlphabet
                     where
                       f k v
                         |    v `notElem` seenSymbols
                           && v /= gapSymbol suppliedAlphabet = IS.singleton k
                         | otherwise = mempty
-}
                   suppliedAlphabet      = alphabet m
                   reducedAlphabet       = suppliedAlphabet
{-
                   reducedAlphabet       =
                       case structure of
                         Additive -> truncateAtMaxSymbol seenSymbols suppliedAlphabet
                         _        ->
                           case alphabetStateNames suppliedAlphabet of
                             [] -> fromSymbols               . reduceTokens $      alphabetSymbols suppliedAlphabet
                             ys -> fromSymbolsWithStateNames . reduceTokens $ zip (alphabetSymbols suppliedAlphabet) ys
                     where
                       reduceTokens = foldMapWithKey (\k v -> if k `oelem` missingSymbolIndicies then [] else [v])
-}
                   reducedTCM =
                       case structure of
                         NonAdditive -> nonAdditiveDistanceFunction
                         Additive    -> additiveDistanceFunction
                         _           -> let !tcm' = tcm -- TCM.reduceTcm missingSymbolIndicies tcm
                                        in (\i j -> toEnum . fromEnum $ tcm' TCM.! (i,j))

        updateMetadataInformation
          :: (Alphabet String, Word -> Word -> Word)
          -> (ParsedCharacter, ParsedCharacterMetadata, TCM, TCMStructure, CharacterName)
          -> (ParsedCharacter, ParsedCharacterMetadata, Word -> Word -> Word, TCMStructure, CharacterName)
        updateMetadataInformation (reducedAlphabet, symbolDistance) (charMay, charMetadata, _, structure, charName) =
            ( charMay
            , charMetadata { alphabet = reducedAlphabet }
            , symbolDistance
            , structure
            , charName
            )

{-
    collapseAndMerge2 :: Foldable f => f (Map String (NonEmpty (Maybe ParsedChar, ParsedCharacterMetadata, TCM, CharacterName))) -> Map String UnifiedSequence
    collapseAndMerge2 = h . g . f
      where
        f :: Foldable f => f (Map String (NonEmpty (Maybe ParsedChar, ParsedCharacterMetadata, TCM, CharacterName))) -> f (Map String (Map OptToken [OptValueWrapper]))
        f = undefined
        g :: Foldable f => f (Map String (Map OptToken [OptValueWrapper])) -> Map String (f (Map OptToken [OptValueWrapper]))
        g = undefined
        h :: Foldable f => Map String (f (Map OptToken [OptValueWrapper])) -> Map String UnifiedSequence
        h = undefined
-}
    collapseAndMerge
      :: Foldable f
      => f (Map String (NonEmpty (ParsedCharacter, ParsedCharacterMetadata, Word -> Word -> Word, TCMStructure, CharacterName)))
      -> Map String UnifiedSequence
    collapseAndMerge = fmap ((MD.fromBlocks *** fromBlocks) . NE.unzip) . fst . foldl' f (mempty, [])
      where
        f :: (Map String (NonEmpty UnifiedBlock), [UnifiedBlock])
          ->  Map String (NonEmpty (ParsedCharacter, ParsedCharacterMetadata, Word -> Word -> Word, TCMStructure, CharacterName))
          -> (Map String (NonEmpty UnifiedBlock), [UnifiedBlock])
        f (prevMapping, prevPad) currTreeChars = (nextMapping, nextPad)
          where
            nextMapping   = inOnlyPrev <> inBoth <> inOnlyCurr
            nextPad       = prevPad <> toList currPad -- generate (length nextMetaData) (const Nothing)

            currPad       = fmap (id *** toMissingCharacters) . head $ toList currMapping
            currMapping   = pure . encodeToBlock <$> currTreeChars

            inBoth        = intersectionWith (<>) prevMapping currMapping-- oldTreeChars nextTreeChars
            inOnlyCurr    =  prepend prevPad  <$> getUnique currMapping prevMapping
            inOnlyPrev    = (<>      currPad) <$> getUnique prevMapping currMapping

            getUnique x y = x `Map.restrictKeys` (lhs `Set.difference` rhs)
              where
                lhs = Set.fromList $ keys x
                rhs = Set.fromList $ keys y

            encodeToBlock :: Foldable1 t
                          => t (ParsedCharacter, ParsedCharacterMetadata, Word -> Word -> Word, TCMStructure, CharacterName)
                          -> UnifiedBlock
            encodeToBlock = (fold1 *** finalizeCharacterBlock) . NE.unzip . foldMap1 encodeBinToSingletonBlock
              where
                encodeBinToSingletonBlock
                  :: (ParsedCharacter, ParsedCharacterMetadata, Word -> Word -> Word, TCMStructure, CharacterName)
                  -> (MetadataBlock () StaticCharacter (Element DynamicChar), PartialCharacterBlock UnifiedContinuousCharacter UnifiedDiscreteCharacter UnifiedDiscreteCharacter UnifiedDiscreteCharacter UnifiedDiscreteCharacter UnifiedDynamicCharacter)
                encodeBinToSingletonBlock (charMay, charMeta, scm, structure, charName) =
                    case charMay of
                      ParsedContinuousCharacter continuousMay ->
                        let mData = MD.continuousToMetadataBlock $ continuousMetadata charName charWeight
                        in (mData, continuousSingleton           . Just .   continuousDecorationInitial charName charWeight $ toContinuousCharacter continuousMay)
                      ParsedDiscreteCharacter     discreteMay ->
                        let mData = MD.discreteToMetadataBlock structure $ discreteMetadataWithTCM charName charWeight specifiedAlphabet scm
                        in (mData,   discreteSingleton structure . Just $ toDiscreteCharacterDecoration charName charWeight specifiedAlphabet scm  staticTransform discreteMay)
                      ParsedDynamicCharacter       dynamicMay ->
                        let mData = MD.dynamicToMetadataBlock $ dynamicMetadataWithTCM charName charWeight specifiedAlphabet scm
                        in (mData,    dynamicSingleton           . Just $  toDynamicCharacterDecoration charName charWeight specifiedAlphabet scm dynamicTransform  dynamicMay)
                  where
                    alphabetLength    = toEnum $ length specifiedAlphabet
                    charWeight        = weight   charMeta
                    specifiedAlphabet = alphabet charMeta
                    missingCharValue  = NE.fromList $ toList specifiedAlphabet
                    dynamicTransform  = maybe (Missing alphabetLength) (encodeStream specifiedAlphabet)
                    staticTransform   = encodeElement specifiedAlphabet . fromMaybe missingCharValue


            -- Necessary for mixing [] with NonEmpty
            prepend :: [a] -> NonEmpty a -> NonEmpty a
            prepend list ne =
              case list of
                []   -> ne
                x:xs -> x :| (xs <> toList ne)


fromTreeOnlyFile :: FracturedParseResult -> Bool
fromTreeOnlyFile fpr = null chars || all null chars
  where
    chars = parsedChars fpr


terminalNames2 :: ReferenceDAG a b (Maybe String) -> [Identifier]
terminalNames2 dag = catMaybes $ (`nodeDatum` dag) <$> leaves dag


gatherForestsTerminalNames :: FracturedParseResult -> ([NonEmpty Identifier], FracturedParseResult)
gatherForestsTerminalNames fpr = (identifiers, fpr)
  where
    identifiers =
        case parsedForests fpr of
          Nothing      -> []
          Just (e:|es) -> catMaybes $ f <$> (e:es)
      where
        f forest =
          case foldMap terminalNames2 forest of
             []   -> Nothing
             x:xs -> Just $ x :| xs


additiveDistanceFunction :: Word -> Word -> Word
additiveDistanceFunction i j = max i j - min i j

nonAdditiveDistanceFunction :: Word -> Word -> Word
nonAdditiveDistanceFunction i j
  | i == j    = 0
  | otherwise = 1
