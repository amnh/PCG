-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Read.Unification.Master
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

module PCG.Command.Types.Read.Unification.Master where

import           Bio.Character
import           Bio.Character.Encodable
--import           Bio.Character.Decoration.Continuous hiding (characterName)
import           Bio.Character.Decoration.Discrete hiding (characterName)
import           Bio.Character.Decoration.Dynamic  hiding (characterName)
import           Bio.Character.Parsed
import           Bio.Sequence                      hiding (hexmap)
import           Bio.Sequence.Block
import           Bio.Metadata.CharacterName        hiding (sourceFile)
import           Bio.Metadata.Parsed
import           Bio.PhyloGraph.Solution           hiding (parsedChars)
import           Bio.PhyloGraph.DAG
import           Bio.PhyloGraph.Forest.Parsed
import           Bio.PhyloGraphPrime
import           Bio.PhyloGraphPrime.Component
import           Bio.PhyloGraphPrime.PhylogeneticDAG
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.ReferenceDAG
import           Control.Arrow                     ((&&&))
import           Control.Applicative               ((<|>))
import           Control.Parallel.Custom
import           Control.Parallel.Strategies
import           Data.Alphabet
import           Data.Bifunctor                    (first)
import           Data.Foldable
import qualified Data.IntSet                as IS
import           Data.Key
import           Data.List                         (transpose, zip4)
import           Data.List.NonEmpty                (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty         as NE
import           Data.List.Utility                 (duplicates)
import           Data.Map                          (Map, intersectionWith, keys)
import qualified Data.Map                   as Map
import           Data.Maybe                        (catMaybes, fromMaybe, listToMaybe)
import           Data.Semigroup                    ((<>), sconcat)
import           Data.Semigroup.Foldable
import           Data.Set                          (Set, (\\))
import qualified Data.Set                   as Set
import           Data.TCM                          (TCM)
import qualified Data.TCM                   as TCM
import           Data.MonoTraversable
import           Data.Vector                       (Vector)
import           PCG.Command.Types.Read.Unification.UnificationError
--import           PCG.SearchState 
import           Prelude                    hiding (lookup, zip, zipWith)

import Debug.Trace


data FracturedParseResult
   = FPR
   { parsedChars   :: TreeChars
   , parsedMetas   :: Vector ParsedCharacterMetadata -- Vector StandardMetadata
   , parsedForests :: ParserForestSet
   , relatedTcm    :: Maybe TCM
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
    

masterUnify' :: [FracturedParseResult] -> Either UnificationError (Solution DAG)
masterUnify' = undefined --rectifyResults


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
      []   -> fmap (fmap riefiedSolution) dagForest --      = undefined -- Right maskedSolution
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
    duplicateNames  = filter (not . (all null) . fst) $ first (fmap duplicates) `parmap'` forestTaxa
    -- Step 6: Assert that each forest's terminal node set is exactly the same as the taxa set from "data files"
    extraNames   :: [([Set Identifier], FracturedParseResult)]
    extraNames      = filter (not . (all null) . fst) $ first (fmap ((\\ taxaSet) . Set.fromList . toList)) `parmap'` forestTaxa
    missingNames :: [([Set Identifier], FracturedParseResult)]
    missingNames    = filter (not . (all null) . fst) $ first (fmap ((taxaSet \\) . Set.fromList . toList)) `parmap'` forestTaxa
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
          (False, False) -> Right . Right . PhylogeneticSolution $ (matchToChars charSeqs) <$> NE.fromList suppliedForests
      where
        defaultCharacterSequenceDatum = fromBlocks . fmap blockTransform . toBlocks . head $ toList charSeqs
          where
            blockTransform = hexmap f f f f f f
            f = const Nothing
        
        singletonComponent (label, datum) = PhylogeneticForest . pure . PDAG $ unfoldDAG rootLeafGen True
          where
            rootLeafGen x
              | x         = (                [], PNode (Just "Trivial Root") defaultCharacterSequenceDatum, [(Nothing, not x)])
              | otherwise = ([(Nothing, not x)], PNode (Just label         )                         datum, []                )

        matchToChars :: Map String UnifiedCharacterSequence
                     -> PhylogeneticForest ParserTree
                     -> PhylogeneticForest UnRiefiedCharacterDAG --CharacterDAG
        matchToChars charMapping = fmap (PDAG . fmap f)
          where
            f label = PNode label $ fromMaybe defaultCharacterSequenceDatum charLabelMay
              where
                charLabelMay     = label >>= (`lookup` charMapping)
{-
              case e of
                  Nothing    -> PNode "HTU" Nothing
                  Just label -> PNode label $ label `lookup` charMapping
-}
-- Omitted from old unifcation process            
--    combinedData    = Solution (HM.fromList $ assocs charSeqs) combinedMetadata dagForests
    -- Step 9:  TODO: Node encoding
--    encodedSolution = encodeSolution combinedData
    -- Step 10: TODO: masking for the nodes
--    maskedSolution  = addMasks encodedSolution

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
    expandForestErrors :: Foldable t => [([t TaxaName], FracturedParseResult)] -> [[(t TaxaName, FracturedParseResult)]]
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
joinSequences2 :: Foldable t => t FracturedParseResult -> Map String UnifiedCharacterSequence
joinSequences2 = collapseAndMerge . reduceAlphabets . deriveCorrectTCMs . deriveCharacterNames
  where
    
    -- We do this to correctly construct the CharacterNames.
    deriveCharacterNames :: Foldable t
                         => t FracturedParseResult
                         -> [ Map String (NonEmpty (Maybe ParsedChar, ParsedCharacterMetadata, Maybe TCM, CharacterName)) ]
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
              where
                correctName [] = Nothing
                correctName ys = Just ys

    deriveCorrectTCMs :: Functor f
                      => f (Map String (NonEmpty (Maybe ParsedChar, ParsedCharacterMetadata, Maybe TCM, CharacterName)))
                      -> f (Map String (NonEmpty (Maybe ParsedChar, ParsedCharacterMetadata,       TCM, CharacterName)))
    deriveCorrectTCMs = fmap (fmap (fmap selectTCM))
      where
        selectTCM (charMay, charMetadata, tcmMay, charName) = (charMay, charMetadata, selectedTCM, charName)
          where
            selectedTCM       = fromMaybe defaultTCM $ tcmMay <|> parsedTCM charMetadata
            specifiedAlphabet = alphabet charMetadata
            defaultTCM        = TCM.generate (length specifiedAlphabet) $ \(i,j) -> (if i == j then 0 else 1 :: Int)

    reduceAlphabets :: Functor f
                    => f (Map String (NonEmpty (Maybe ParsedChar, ParsedCharacterMetadata, TCM, CharacterName)))
                    -> f (Map String (NonEmpty (Maybe ParsedChar, ParsedCharacterMetadata, TCM, CharacterName)))
    reduceAlphabets = fmap reduceFileBlock
      where
        reduceFileBlock mapping = fmap (zipWith removeExtraneousSymbols observedSymbolSets) mapping
          where
            observedSymbolSets :: NonEmpty (Set String)
            observedSymbolSets = fmap (foldMap f) . NE.fromList . transpose . fmap toList $ toList mapping
             where
               f (x,_,_,_) = foldMap (foldMap (Set.fromList . toList)) x

        removeExtraneousSymbols :: Set String
                                -> (Maybe ParsedChar, ParsedCharacterMetadata, TCM, CharacterName)
                                -> (Maybe ParsedChar, ParsedCharacterMetadata, TCM, CharacterName)
        removeExtraneousSymbols observedSymbols input@(charMay, charMetadata, tcm, charName)
          | isDynamic charMetadata      = input
          | onull missingSymbolIndicies = input
          | otherwise                   = (charMay, charMetadata { alphabet = reducedAlphabet }, reducedTCM, charName)
          where
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
                      TCM.Additive -> \(i,j) -> toEnum . abs $ i - j
                      _            -> f
                
                f (i,j) = tcm TCM.! (i', j')
                  where
                    i' = i + iOffset
                    j' = j + jOffset
                    xs = otoList missingSymbolIndicies
                    iOffset = length $ filter (<=i) xs
                    jOffset = length $ filter (<=j) xs

    collapseAndMerge = fmap fromBlocks . fst . foldl' f (mempty, [])
      where
        f :: (Map String (NonEmpty UnifiedCharacterBlock), [UnifiedCharacterBlock])
          ->  Map String (NonEmpty (Maybe ParsedChar, ParsedCharacterMetadata, TCM, CharacterName))
          -> (Map String (NonEmpty UnifiedCharacterBlock), [UnifiedCharacterBlock])
        f (prevMapping, prevPad) currTreeChars = (nextMapping, nextPad)
          where
            nextMapping    = inOnlyPrev <> inBoth <> inOnlyCurr
            nextPad        = prevPad <> toList currPad -- generate (length nextMetaData) (const Nothing)

            currPad        = fmap toMissingCharacters . head $ toList currMapping
            currMapping    = pure . encodeToBlock <$> currTreeChars

            inBoth         = intersectionWith (<>) prevMapping currMapping-- oldTreeChars nextTreeChars
            inOnlyCurr     =  prepend prevPad  <$> getUnique currMapping prevMapping
            inOnlyPrev     = (<>      currPad) <$> getUnique prevMapping currMapping
        
            getUnique x y = x `Map.restrictKeys` (lhs `Set.difference` rhs)
              where
                lhs = Set.fromList $ keys x
                rhs = Set.fromList $ keys y

            encodeToBlock :: Foldable1 t
                          => t (Maybe ParsedChar, ParsedCharacterMetadata, TCM, CharacterName)
                          -> UnifiedCharacterBlock
            encodeToBlock = foldMap1 encodeBinToSingletonBlock
              where
                encodeBinToSingletonBlock :: (Maybe ParsedChar, ParsedCharacterMetadata, TCM, CharacterName)
                                          -> UnifiedCharacterBlock
                encodeBinToSingletonBlock (charMay, charMeta, tcm, charName)
                  | isDynamic charMeta = dynamicSingleton     dynamicCharacter
                  | otherwise          = discreteSingleton tcm staticCharacter
                  where
                    alphabetLength    = length specifiedAlphabet
                    specifiedAlphabet = alphabet charMeta
                    charWeight        = weight   charMeta
                    missingCharValue  = NE.fromList $ toList specifiedAlphabet
                    staticTransform   = encodeElement specifiedAlphabet . maybe missingCharValue NE.head
                    staticCharacter   = Just $ toDiscreteCharacterDecoration charName charWeight specifiedAlphabet tcm staticTransform charMay
                    dynamicTransform  = maybe (Missing alphabetLength) (encodeStream specifiedAlphabet)
                    dynamicCharacter  = Just $ toDynamicCharacterDecoration  charName charWeight specifiedAlphabet tcm dynamicTransform charMay
                        

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


terminalNames2 :: ReferenceDAG (Maybe Double) (Maybe String) -> [Identifier]
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
