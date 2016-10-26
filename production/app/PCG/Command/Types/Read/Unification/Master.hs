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

-- TODO: I was forced to hard code DynamicChar, because it's hard coded in Bio.Phylograph.Node.Internal --> Node.encoded

module PCG.Command.Types.Read.Unification.Master where

import           Bio.Metadata             hiding (name)
import           Bio.Metadata.MaskGenerator
import           Bio.PhyloGraph.Solution  hiding (parsedChars)
import           Bio.PhyloGraph.DAG
import           Bio.PhyloGraph.Forest
import           Bio.PhyloGraph.Node      hiding (isLeaf)
import           Bio.Character.Dynamic
import           Bio.Character.Parsed
import           Control.Arrow                   ((***),(&&&))
import           Data.Bifunctor                  (first)
import           Data.Foldable
import qualified Data.HashMap.Lazy        as HM
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.List.NonEmpty       as NE  (fromList)
import           Data.List.Utility               (duplicates)
import           Data.Map                        (assocs, difference, intersectionWith, keys)
import           Data.Maybe                      (catMaybes, fromJust)
import           Data.Semigroup                  ((<>))
import           Data.Set                        ((\\))
import qualified Data.Set                 as S   (fromList)
import           Data.Vector                     (Vector, (//), generate)
import qualified Data.Vector              as V   (find, zipWith)
import           File.Format.Newick
import           File.Format.TransitionCostMatrix
import           PCG.Command.Types.Read.Unification.UnificationError

--import Debug.Trace

data FracturedParseResult
   = FPR
   { parsedChars :: TreeChars
   , parsedMetas :: Vector StandardMetadata
   , parsedTrees :: Forest NewickNode
   , relatedTcm  :: Maybe TCM
   , sourceFile  :: FilePath
   } deriving (Show)


masterUnify' :: [FracturedParseResult] -> Either UnificationError (Solution DAG)
masterUnify' = rectifyResults


rectifyResults :: [FracturedParseResult] -> Either UnificationError (Solution DAG)
rectifyResults fprs =
  case errors of
    [] -> Right {- $ (\x -> trace ("Called one (maybe?) " <> show x) x) -} maskedSolution
    xs -> Left $ foldl1 (<>) xs
  where
    -- Step 1: Gather data file contents
    dataSeqs        = (parsedChars &&& parsedMetas) <$> filter (not . fromTreeOnlyFile) fprs
    -- Step 2: Union the taxa names together into total terminal set
    taxaSet         = mconcat $ (S.fromList . keys . fst) <$> dataSeqs
    -- Step 3: Gather forest file data
    allForests      = filter (not . null . parsedTrees) fprs
    -- Step 4: Gather the taxa names for each forest from terminal nodes
    forestTaxa      = (mconcat . fmap terminalNames . parsedTrees &&& id) <$> allForests
    -- Step 5: Assert that each terminal node name is unique in the forest
    duplicateNames  = filter (not . null . fst) $ first duplicates <$> forestTaxa
    -- Step 6: Assert that each forest's terminal node set is exactly the same as the taxa set from "data files"
    extraNames      = filter (not . null . fst) $ first ((\\ taxaSet) . S.fromList) <$> forestTaxa
    missingNames    = filter (not . null . fst) $ first ((taxaSet \\) . S.fromList) <$> forestTaxa
    -- Step 7: Combine disparte sequences from many sources  into single metadata & character sequence.
    (charSeqs,combinedMetadata) = joinSequences dataSeqs
    -- Step 8: Convert topological forests to DAGs (using reference indexing from #7 results)
    dagForests      = fromNewick . parsedTrees <$> allForests
    combinedData    = Solution (HM.fromList $ assocs charSeqs) combinedMetadata dagForests
    -- Step 9:  TODO: Node encoding
    encodedSolution = encodeSolution combinedData
    -- Step 10: TODO: masking for the nodes
    maskedSolution  = addMasks encodedSolution

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


fromTreeOnlyFile :: FracturedParseResult -> Bool
fromTreeOnlyFile fpr = null chars || all null chars
  where
    chars = parsedChars fpr


terminalNames :: NewickNode -> [Identifier]
terminalNames n
  | isLeaf n  = [fromJust $ newickLabel n]
  | otherwise = mconcat $ terminalNames <$> descendants n


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
        matching = V.find (\n -> name n == inName) $ nodes inDAG


-- | Joins the sequences of a fractured parse result
joinSequences :: Foldable t => t (TreeChars, Vector StandardMetadata) -> (TreeChars, Vector StandardMetadata)
joinSequences =  foldl' g (mempty, mempty)
  where
--    f :: (TreeChars, Vector StandardMetadata) -> FracturedParseResult -> (TreeChars, Vector StandardMetadata)
--    f acc fpr = g acc $ (parsedMetas fpr, parsedChars fpr)

    g :: (TreeChars, Vector StandardMetadata) -> (TreeChars, Vector StandardMetadata) -> (TreeChars, Vector StandardMetadata)
    g (oldTreeChars, oldMetaData) (nextTreeChars, nextMetaData) = (inOnlyOld `mappend` inBoth `mappend` inOnlyNext, oldMetaData `mappend` nextMetaData)
      where
        oldPad       = generate (length  oldMetaData) (const Nothing)
        nextPad      = generate (length nextMetaData) (const Nothing)
        inBoth       = intersectionWith mappend oldTreeChars nextTreeChars
        inOnlyOld    = fmap (`mappend` nextPad) $  oldTreeChars `difference` nextTreeChars
        inOnlyNext   = fmap (oldPad  `mappend`) $ nextTreeChars `difference` oldTreeChars


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


-- | Wrapper for encoding
encodeIt :: ParsedChars -> Vector StandardMetadata -> Vector DynamicChar
encodeIt = V.zipWith encodeOverMetadata

