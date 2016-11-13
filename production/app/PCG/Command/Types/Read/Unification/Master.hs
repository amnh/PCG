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
import           Bio.Character.Dynamic
import           Bio.Character.Parsed
import           Bio.Sequence
import           Bio.Sequence.Block
--import           Bio.Metadata
import           Bio.Metadata.CharacterName hiding (sourceFile)
import           Bio.Metadata.Parsed
import           Bio.Metadata.MaskGenerator
import           Bio.PhyloGraph.Solution    hiding (parsedChars)
import           Bio.PhyloGraph.DAG
import           Bio.PhyloGraph.Forest
import           Bio.PhyloGraph.Forest.Parsed
import           Bio.PhyloGraph.Node               (encoded, nodeIdx)
import qualified Bio.PhyloGraph.Node        as Node
import           Bio.PhyloGraphPrime
import           Bio.PhyloGraphPrime.Component
import           Bio.PhyloGraphPrime.Node
import           Bio.PhyloGraphPrime.ReferenceDAG
import           Control.Arrow                     ((***), (&&&))
import           Control.Applicative               ((<|>))
import           Data.Alphabet
import           Data.Bifunctor                    (first)
import           Data.Foldable
import qualified Data.HashMap.Lazy          as HM
import qualified Data.IntSet                as IS
import           Data.Key
import           Data.List                         (transpose, zip4)
import           Data.List.NonEmpty                (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty         as NE  (fromList)
import           Data.List.Utility                 (duplicates)
import           Data.Map                          (Map, assocs, difference, intersectionWith, keys)
import qualified Data.Map                   as Map
import           Data.Maybe                        (catMaybes, fromJust, fromMaybe)
import           Data.Semigroup                    ((<>), sconcat)
import           Data.Semigroup.Foldable
import           Data.Set                          (Set, (\\))
import qualified Data.Set                   as Set
import           Data.TCM                          (TCM)
import qualified Data.TCM                   as TCM
import           Data.MonoTraversable
import           Data.Vector                       (Vector, (//))
import qualified Data.Vector                as V
import           File.Format.Newick
-- import           File.Format.TransitionCostMatrix
import           PCG.Command.Types.Read.Unification.UnificationError
import           Prelude                   hiding  (lookup, zip, zipWith)

-- import Debug.Trace

-- type TreeChars      = Map String ParsedChars
-- type ParsedChars    = Vector (Maybe ParsedChar)
-- type ParsedChar     = Vector AmbiguityGroup
-- type AmbiguityGroup = [String]

data FracturedParseResult
   = FPR
   { parsedChars :: TreeChars
   , parsedMetas :: Vector ParsedCharacterMetadata -- Vector StandardMetadata
   , parsedTrees :: ParserForest
   , relatedTcm  :: Maybe TCM
   , sourceFile  :: FilePath
   }


instance Show FracturedParseResult where
    show fpr = unlines 
        [ "FPR"
        , "  { parsedChars = " <> show (parsedChars fpr)
        , "  , parsedMetas = " <> show (parsedMetas fpr)
        , "  , parsedTrees = " <> if null (parsedTrees fpr) then "Nothing" else "Just <trees>"
        , "  , relatedTcm  = " <> show (relatedTcm  fpr)
        , "  , sourceFile  = " <> show (sourceFile  fpr)
        , "  }"
        ]


type TopologicalResult = PhylogeneticSolution (ReferenceDAG (Maybe Double) (Maybe String))


type CharacterResult   = PhylogeneticSolution
                           (ReferenceDAG
                             (Maybe Double)
                             (PhylogeneticNode
                               (Maybe
                                 (CharacterSequence StaticCharacterBlock DynamicChar))
                             )
                           )


masterUnify' :: [FracturedParseResult] -> Either UnificationError (Solution DAG)
masterUnify' = undefined --rectifyResults


-- |
-- Unify disparate parsed results into a single phylogenetic solution.
rectifyResults2 :: [FracturedParseResult]
                -> Either UnificationError (Either TopologicalResult CharacterResult)
rectifyResults2 fprs =
    case errors of
      []   -> dagForest --      = undefined -- Right maskedSolution
      x:xs -> Left . sconcat $ x:|xs
  where
    -- Step 1: Gather data file contents
    dataSeqs        = filter (not . fromTreeOnlyFile) fprs
    -- Step 2: Union the taxa names together into total terminal set
    taxaSet         = mconcat $ (Set.fromList . keys . parsedChars) <$> dataSeqs
    -- Step 3: Gather forest file data
    allForests      = filter (not . null . parsedTrees) fprs
    -- Step 4: Gather the taxa names for each forest from terminal nodes
    forestTaxa      = (foldMap (foldMap terminalNames2) . parsedTrees &&& id) <$> allForests
    -- Step 5: Assert that each terminal node name is unique in the forest
    duplicateNames  = filter (not . null . fst) $ first duplicates <$> forestTaxa
    -- Step 6: Assert that each forest's terminal node set is exactly the same as the taxa set from "data files"
    extraNames      = filter (not . null . fst) $ first ((\\ taxaSet) . Set.fromList) <$> forestTaxa
    missingNames    = filter (not . null . fst) $ first ((taxaSet \\) . Set.fromList) <$> forestTaxa
    -- Step 7: Combine disparte sequences from many sources  into single metadata & character sequence.
    charSeqs        = joinSequences2 dataSeqs
    -- Step 8: Collect the parsed forests to be merged
    suppliedForests = catMaybes $ parsedTrees <$> allForests
      
    -- Step 9: Convert topological forests to DAGs (using reference indexing from #7 results)
    dagForest       =
        case (null suppliedForests, null charSeqs) of
          -- Throw a unification error here
          (True , True ) -> Left . UnificationError . pure . VacuousInput $ sourceFile <$> NE.fromList fprs
          -- Build a default forest of singleton components
          (True , False) -> Right . Right . PhylogeneticSolution . pure
                          . foldMap1 singletonComponent . NE.fromList $ toKeyedList charSeqs
          -- Build a forest of with Units () as character type parameter
          (False, True ) -> Right . Left . PhylogeneticSolution . pure
                          . sconcat $ NE.fromList suppliedForests
          -- BUild a forest with the corresponding character data on the nodes
          (False, False) -> Right . Right $ PhylogeneticSolution . pure
                          . foldMap1 (matchToChars charSeqs) $ NE.fromList suppliedForests
      where
        
        singletonComponent (label, datum) = PhylogeneticForest . pure $ unfoldDAG rootLeafGen True
          where
            rootLeafGen x
              | x         = (                [], PNode "Trivial Root" Nothing    , [(Nothing, not x)])
              | otherwise = ([(Nothing, not x)], PNode label         (Just datum), []                )

        matchToChars :: Map String (CharacterSequence StaticCharacterBlock DynamicChar)
                     -> PhylogeneticForest ParserTree
                     -> PhylogeneticForest (ReferenceDAG (Maybe Double) (PhylogeneticNode (Maybe (CharacterSequence StaticCharacterBlock DynamicChar))))
        matchToChars charMapping = fmap (fmap f)
          where
            f e =
                case e of
                  Nothing    -> PNode "HTU" Nothing
                  Just label -> PNode label $ label `lookup` charMapping

-- Omitted from old unifcation process            
--    combinedData    = Solution (HM.fromList $ assocs charSeqs) combinedMetadata dagForests
    -- Step 9:  TODO: Node encoding
--    encodedSolution = encodeSolution combinedData
    -- Step 10: TODO: masking for the nodes
--    maskedSolution  = addMasks encodedSolution

    -- Error collection
    errors          = catMaybes [duplicateError, extraError, missingError]
    duplicateError  = colateErrors ForestDuplicateTaxa duplicateNames
    extraError      = colateErrors ForestExtraTaxa     extraNames
    missingError    = colateErrors ForestMissingTaxa   missingNames

    colateErrors :: (Foldable t, Foldable t')
                 => ((NonEmpty a) -> FilePath -> UnificationErrorMessage)
                 -> t (t' a, FracturedParseResult)
                 -> Maybe UnificationError
    colateErrors f xs =
      case toList xs of
        [] -> Nothing
        ys -> Just . UnificationError . NE.fromList $ transformFPR <$> ys
      where
        transformFPR (x,y) = f (NE.fromList $ toList x) $ sourceFile y


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
joinSequences2 :: Foldable t => t FracturedParseResult -> Map String (CharacterSequence StaticCharacterBlock DynamicChar)
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
            missingSymbolIndicies = foldMapWithKey (\k v -> if v `notElem` observedSymbols then IS.singleton k else mempty) suppliedAlphabet
            suppliedAlphabet      = alphabet charMetadata
            reducedAlphabet       =
                case alphabetStateNames suppliedAlphabet of
                  [] -> fromSymbols               . reduceTokens $     (alphabetSymbols suppliedAlphabet)
                  xs -> fromSymbolsWithStateNames . reduceTokens $ zip (alphabetSymbols suppliedAlphabet) xs
              where
                reduceTokens = foldMapWithKey (\k v -> if k `oelem` missingSymbolIndicies then [] else [v])
            reducedTCM = TCM.generate (TCM.size tcm - olength missingSymbolIndicies) f
              where
                f (i,j) = tcm TCM.! (i', j')
                  where
                    i' = i + iOffset
                    j' = j + jOffset
                    xs = otoList missingSymbolIndicies
                    iOffset = length $ filter (<=i) xs
                    jOffset = length $ filter (<=j) xs

    collapseAndMerge = fmap fromBlocks . fst . foldl' f (mempty, [])
      where
        f :: (Map String (NonEmpty (CharacterBlock StaticCharacterBlock DynamicChar)), [CharacterBlock StaticCharacterBlock DynamicChar])
          ->  Map String (NonEmpty (Maybe ParsedChar, ParsedCharacterMetadata, TCM, CharacterName))
          -> (Map String (NonEmpty (CharacterBlock StaticCharacterBlock DynamicChar)), [CharacterBlock StaticCharacterBlock DynamicChar])
        f (prevMapping, prevPad) currTreeChars = (nextMapping, nextPad)
          where
            nextMapping    = inOnlyPrev <> inBoth <> inOnlyCurr
            nextPad        = prevPad <> toList currPad -- generate (length nextMetaData) (const Nothing)

            currPad        = fmap toMissingCharacters . head $ toList currMapping
            currMapping    = pure . encodeToBlock <$> currTreeChars

            inBoth         = intersectionWith (<>) prevMapping currMapping-- oldTreeChars nextTreeChars
            inOnlyCurr     = (prepend prevPad) <$> getUnique currMapping prevMapping
            inOnlyPrev     = (<>      currPad) <$> getUnique prevMapping currMapping
        
            getUnique x y = x `Map.restrictKeys` (lhs `Set.difference` rhs)
              where
                lhs = Set.fromList $ keys x
                rhs = Set.fromList $ keys y

            encodeToBlock :: Foldable1 t
                          => t (Maybe ParsedChar, ParsedCharacterMetadata, TCM, CharacterName)
                          -> CharacterBlock StaticCharacterBlock DynamicChar
            encodeToBlock = foldMap1 encodeBinToSingletonBlock
              where
                encodeBinToSingletonBlock :: (Maybe ParsedChar, ParsedCharacterMetadata, TCM, CharacterName)
                                          -> CharacterBlock StaticCharacterBlock DynamicChar
                encodeBinToSingletonBlock (charMay, charMeta, tcm, charName)
                  | isDynamic charMeta = dynamicSingleton  specifiedAlphabet charName tcm (encodeStream specifiedAlphabet) charMay
                  | otherwise          = discreteSingleton specifiedAlphabet charName tcm staticTransform charMay
                  where
                    specifiedAlphabet = alphabet charMeta
                    missingCharValue  = pure . NE.fromList $ toList specifiedAlphabet
                    staticTransform   = encodeStream specifiedAlphabet . fromMaybe missingCharValue

            -- Necisarry for mixing [] with NonEmpty
            prepend :: [a] -> NonEmpty a -> NonEmpty a
            prepend list ne =
              case list of
                []   -> ne
                x:xs -> x :| (xs <> toList ne) 

{-
        encodeCharacters :: (Foldable t, Foldable t') => t StandardMetadata -> t' (Maybe ParsedChar) -> CharacterBlock StaticCharacter DynamicChar
        encodeCharacters metadatas chars
          | length metadatas /= length chars = error
          | otherwise = zipWith f (toList metadatas) (toList chars)
          where
            f m c = 
-}







{-

rectifyResults :: [FracturedParseResult] -> Either UnificationError (Solution DAG)
rectifyResults fprs =
  case errors of
    [] -> Right maskedSolution
    xs -> Left . sconcat $ NE.fromList xs
  where
    -- Step 1: Gather data file contents
    dataSeqs        = (parsedChars &&& parsedMetas) <$> filter (not . fromTreeOnlyFile) fprs
    -- Step 2: Union the taxa names together into total terminal set
    taxaSet         = mconcat $ (Set.fromList . keys . fst) <$> dataSeqs
    -- Step 3: Gather forest file data
    allForests      = filter (not . null . parsedTrees) fprs
    -- Step 4: Gather the taxa names for each forest from terminal nodes
    forestTaxa      = (mconcat . fmap terminalNames . parsedTrees &&& id) <$> allForests
    -- Step 5: Assert that each terminal node name is unique in the forest
    duplicateNames  = filter (not . null . fst) $ first duplicates <$> forestTaxa
    -- Step 6: Assert that each forest's terminal node set is exactly the same as the taxa set from "data files"
    extraNames      = filter (not . null . fst) $ first ((\\ taxaSet) . Set.fromList) <$> forestTaxa
    missingNames    = filter (not . null . fst) $ first ((taxaSet \\) . Set.fromList) <$> forestTaxa
    -- Step 7: Combine disparte sequences from many sources  into single metadata & character sequence.
    (charSeqs,combinedMetadata) = joinSequences dataSeqs
    -- Step 8: Convert topological forests to DAGs (using reference indexing from #7 results)
    dagForests      = fromNewick . parsedTrees <$> allForests
    combinedData    = Solution (HM.fromList $ assocs charSeqs) combinedMetadata dagForests
    -- Step 9:  TODO: Node encoding
    encodedSolution = encodeSolution combinedData
    -- Step 10: TODO: masking for the nodes
    maskedSolution  = addMasks encodedSolution

    -- Error collection
    errors          = catMaybes [duplicateError, extraError, missingError]
    duplicateError  = colateErrors ForestDuplicateTaxa duplicateNames
    extraError      = colateErrors ForestExtraTaxa     duplicateNames
    missingError    = colateErrors ForestMissingTaxa   duplicateNames

    colateErrors :: ((NonEmpty a) -> FilePath -> UnificationErrorMessage) -> [([a], FracturedParseResult)] -> Maybe UnificationError
    colateErrors f xs =
      case xs of
        [] -> Nothing
        ys -> Just . UnificationError . NE.fromList $ transformFPR <$> ys
      where
        transformFPR (x,y) = f (NE.fromList $ toList x) $ sourceFile y

-}

fromTreeOnlyFile :: FracturedParseResult -> Bool
fromTreeOnlyFile fpr = null chars || all null chars
  where
    chars = parsedChars fpr


terminalNames :: NewickNode -> [Identifier]
terminalNames n
  | isLeaf n  = [fromJust $ newickLabel n]
  | otherwise = mconcat $ terminalNames <$> descendants n


terminalNames2 :: ReferenceDAG (Maybe Double) (Maybe String) -> [Identifier]
terminalNames2 dag = catMaybes $ (`nodeDatum` dag) <$> leaves dag

{-
-- | Functionality to encode into a solution
encodeSolution :: StandardSolution -> StandardSolution
encodeSolution inVal@(Solution taxaSeqs metadataInfo inForests) = inVal {forests = HM.foldrWithKey encodeAndSet inForests taxaSeqs}
  where
    encodeAndSet :: Identifier -> Sequences -> [Forest DAG] -> [Forest DAG]
    encodeAndSet taxonName inSeqs = fmap (fmap (applyToDAG taxonName coded))
      where coded = encodeIt inSeqs metadataInfo

    -- TODO: Fix Indexing error in here.
    --       Resulted from running bantu-big-no-tnt data set
    applyToDAG :: Identifier -> Vector DynamicChar -> DAG -> DAG
    applyToDAG inName coded inDAG =
      case matching of
        Nothing    -> inDAG
        Just match -> inDAG {nodes = nodes inDAG // [(nodeIdx match, match {encoded = coded})]}
      where
        matching = V.find (\n -> Node.name n == inName) $ nodes inDAG
-}


-- | Joins the sequences of a fractured parse result
joinSequences :: Foldable t => t (TreeChars, Vector StandardMetadata) -> (TreeChars, Vector StandardMetadata)
joinSequences =  foldl' g (mempty, mempty)
  where
--    f :: (TreeChars, Vector StandardMetadata) -> FracturedParseResult -> (TreeChars, Vector StandardMetadata)
--    f acc fpr = g acc $ (parsedMetas fpr, parsedChars fpr)

    g :: (TreeChars, Vector StandardMetadata) -> (TreeChars, Vector StandardMetadata) -> (TreeChars, Vector StandardMetadata)
    g (oldTreeChars, oldMetaData) (nextTreeChars, nextMetaData) = (inOnlyOld `mappend` inBoth `mappend` inOnlyNext, oldMetaData `mappend` nextMetaData)
      where
        oldPad     = V.generate (length  oldMetaData) (const Nothing)
        nextPad    = V.generate (length nextMetaData) (const Nothing)
        inBoth     = intersectionWith mappend oldTreeChars nextTreeChars
        inOnlyOld  = fmap (`mappend` nextPad) $  oldTreeChars `difference` nextTreeChars
        inOnlyNext = fmap (oldPad  `mappend`) $ nextTreeChars `difference` oldTreeChars


{-
-- | Function to encode given metadata information
-- TODO: Remove tight coupling of DynamicChar here
encodeOverMetadata :: Maybe ParsedChar -> StandardMetadata -> DynamicChar
encodeOverMetadata maybeInChar inMeta =
    case maybeInChar of
      Nothing     -> encodeDynamic (alphabet inMeta) [[]] -- This is bad!!!
      Just inChar ->
        case toList inChar of
          [] -> error "You have an empty character, because that makes sense..."
          xs -> 
            if any null xs
            then error "An empty ambiguity group exisits in an input character :("
            else encodeStream  (alphabet inMeta) . NE.fromList $ NE.fromList <$> xs
-}


{-
-- | Wrapper for encoding
encodeIt :: ParsedChars -> Vector StandardMetadata -> Vector DynamicChar
encodeIt = V.zipWith encodeOverMetadata
-}
