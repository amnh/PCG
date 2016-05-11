-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Report.ImpliedalignmentFasta
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard algorithm for implied alignment
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Types.Report.ImpliedAlignmentFasta where

import Analysis.ImpliedAlignment.Internal
import Bio.Character.Dynamic.Coded
import Bio.Metadata   hiding (name)
import Bio.PhyloGraph.DAG
import Bio.PhyloGraph.Node
import Bio.PhyloGraph.Solution
import Data.Alphabet
import Data.Foldable
import Data.IntMap           (IntMap,insert)
import Data.Key
import Data.List.Utility     (chunksOf)
import Data.Map              (Map, singleton)
import Data.Monoid           ((<>))
import Data.Vector           (Vector)
-- TODO: Remove this import when Data.Vector.Instances gets fixed
import qualified Data.Vector as V ((!))
import Data.Vector.Instances ()
import Prelude        hiding (zipWith)

--iaOutput :: (MetadataSolution s m, GeneralSolution s f) => AlignmentSolution DynamicChar -> s -> [(FilePath, String)]
iaOutput :: AlignmentSolution DynamicChar -> StandardSolution -> [(FilePath, String)]
--iaOutput align solution | trace (mconcat [show align, show solution]) False = undefined
iaOutput align solution = foldMapWithKey characterToFastaFile dynamicCharacterIndiciesAndAlphabets 
  where
    -- Here we use the metadata to filter for dynamic character indicies and
    -- thier corresponding alphabets. 
    dynamicCharacterIndiciesAndAlphabets :: IntMap (Alphabet String)
    dynamicCharacterIndiciesAndAlphabets = foldlWithKey dynamicCharFilter mempty (getMetadata solution)
      where
        dynamicCharFilter im i e = if getType e == DirectOptimization
                                   then insert i (getAlphabet e) im
                                   else im

    -- Here we perform a recursive structure "zip" between graph of forests of
    -- DAGs of nodes containing implied alignment sequences and the original
    -- graph of forests of DAGs of nodes containing the unaligned sequences.
    -- The result of the structure zip is a Map from the taxon name within the
    -- original (right-hand) structure to the implied alignment sequences within
    -- the new (left-hand) structure.
    nodeCharacterMapping :: Map String (Vector DynamicChar)
    nodeCharacterMapping = mconcat $ zipWith f align (getForests solution)
      where
        f alignedForest solutionForest = mconcat $ zipWith g alignedForest solutionForest
        g alignment     dag            = mconcat $ zipWith h (toList alignment) (toList $ getNodes dag)
        h characters    nodeInfo       = singleton (name nodeInfo) characters

    -- The folding function for consuming the 'dynamicCharacterIndiciesAndAlphabets'
    -- structure above. For each character index and corresponding alphabet this
    -- function will create the file name and the file contents in FASTA format.
    -- Internally folds over the 'nodeCharacterMapping' structure to place all
    -- taxa in the graph into the resulting file.
    --
    -- Because 'dynamicCharacterIndiciesAndAlphabets' contains only dynamic
    -- characters, the resulting list of file content will contain a exactly one
    -- filt content tuple for each dynamic character.
    --
    -- Type checker doesn't like the Int in this explicit type signature.
    -- The type checker can infer the correct (complicated) type alby itself,
    -- so we will let  it do that rather than listen to it complain.
    
    -- characterToFastaFile :: Int -> Alphabet -> [(FilePath, String)]
    characterToFastaFile i alpha = [(characterFileName, foldMapWithKey f nodeCharacterMapping)]
      where
        characterFileName = mconcat ["Character", show i, ".fasta"]
        f nodeName characters = unlines $ titleLine : sequenceLines <> [""]
          where
            titleLine     = "> " <> nodeName
            sequenceLines = chunksOf 50 . concatMap renderAmbiguityGroup . toList . decodeDynamic alpha $ characters V.! i
            renderAmbiguityGroup [x] = show x
            renderAmbiguityGroup xs  = "[" <> concatMap show xs <> "]"
            
