-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Read.Unification.Master
-- Copyright   :  (c) 2015-2015 Ward Wheeler
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

import           Bio.Phylogeny.Solution  hiding (parsedChars)
import           Bio.Metadata hiding (name)
import           Bio.Sequence.Coded
import           Bio.Sequence.Parsed
import           Bio.Phylogeny.Node hiding (isLeaf)
import           Control.Arrow                  ((***),(&&&))
import           Data.Bifunctor                 (first)
--import           Data.BitVector          hiding (not, foldr)
import           Data.Foldable
import qualified Data.HashMap.Lazy       as HM
import qualified Data.List.NonEmpty      as NE (fromList)
import           Data.List.Utility             (duplicates)
import           Bio.Metadata.MaskGenerator
import           Data.Map                      (assocs, difference, intersectionWith, keys)
import           Data.Maybe                    (catMaybes, fromJust)
import           Data.Semigroup                ((<>))
import           Data.Set                      ((\\))
import qualified Data.Set                as S  (fromList)
import           Data.Vector                   (Vector, (//), generate)
import qualified Data.Vector             as V  (find, zipWith)
import           File.Format.Newick
import           File.Format.TransitionCostMatrix
import           PCG.Command.Types.Read.Unification.UnificationError

--import Debug.Trace

data FracturedParseResult
   = FPR
   { parsedChars  :: TreeSeqs
   , parsedMetas  :: Vector StandardMetadata
   , parsedTrees  :: Forest NewickNode
   , relatedTcm   :: Maybe TCM
   , sourceFile   :: FilePath
   } deriving (Show)

masterUnify' :: [FracturedParseResult] -> Either UnificationError (Solution DAG)
masterUnify' = rectifyResults

rectifyResults :: [FracturedParseResult] -> Either UnificationError (Solution DAG)
rectifyResults fprs
  | not (null errors) = Left  $ foldl1 (<>) errors
  | otherwise         = Right {- $ (\x -> trace ("Called one (maybe?) " <> show x) x) -} maskedSolution
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
    duplicateError  =
      if null duplicateNames
      then Nothing
      else Just . UnificationError . NE.fromList $ uncurry ForestDuplicateTaxa . (NE.fromList . toList *** sourceFile) <$> duplicateNames
    extraError =
      if null extraNames
      then Nothing
      else Just . UnificationError . NE.fromList $ uncurry ForestExtraTaxa     . (NE.fromList . toList *** sourceFile) <$> extraNames
    missingError =
      if null missingNames
      then Nothing
      else Just . UnificationError . NE.fromList $ uncurry ForestMissingTaxa   . (NE.fromList . toList *** sourceFile) <$> missingNames

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
    encodeAndSet taxonName s = fmap (fmap (applyToDAG taxonName coded))
      where coded = encodeIt s metadataInfo

    applyToDAG :: Identifier -> EncodedSequences -> DAG -> DAG
    applyToDAG inName coded inD =
      case matching of
        Nothing    -> inD
        Just match -> inD {nodes = nodes inD // [(code match, match {encoded = coded})]}
      where
        matching = V.find (\n -> name n == inName) $ nodes inD


-- | Joins the sequences of a fractured parse result
joinSequences :: Foldable t => t (TreeSeqs, Vector StandardMetadata) -> (TreeSeqs, Vector StandardMetadata)
joinSequences =  foldl' g (mempty, mempty)
  where
--    f :: (TreeSeqs, Vector StandardMetadata) -> FracturedParseResult -> (TreeSeqs, Vector StandardMetadata)
--    f acc fpr = g acc $ (parsedMetas fpr, parsedChars fpr)

    g :: (TreeSeqs, Vector StandardMetadata) -> (TreeSeqs, Vector StandardMetadata) -> (TreeSeqs, Vector StandardMetadata)
    g (oldTreeSeqs, oldMetaData) (nextTreeSeqs, nextMetaData) = (inOnlyOld `mappend` inBoth `mappend` inOnlyNext, oldMetaData `mappend` nextMetaData)
      where
        oldPad       = generate (length  oldMetaData) (const Nothing) 
        nextPad      = generate (length nextMetaData) (const Nothing)
        inBoth       = intersectionWith mappend oldTreeSeqs nextTreeSeqs
        inOnlyOld    = fmap (`mappend` nextPad) $  oldTreeSeqs `difference` nextTreeSeqs
        inOnlyNext   = fmap (oldPad  `mappend`) $ nextTreeSeqs `difference`  oldTreeSeqs

-- | Function to encode given metadata information
encodeOverMetadata :: ParsedSeq -> StandardMetadata -> EncodedSeq
encodeOverMetadata inSeq inMetadata = encodeOverAlphabet inSeq (alphabet inMetadata)

-- | Wrapper for encoding
encodeIt :: ParsedSequences -> Vector StandardMetadata -> EncodedSequences
encodeIt = V.zipWith (\s info -> (`encodeOverMetadata` info) =<< s)
