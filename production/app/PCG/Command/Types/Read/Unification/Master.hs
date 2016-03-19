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

import Prelude hiding ((++))

import PCG.Command.Types.Read.Unification.UnificationError
import Bio.Phylogeny.Graph.Parsed
import Control.Monad
import Data.List.NonEmpty (fromList)
import Bio.Metadata.Class
import Bio.Sequence.Parsed.Class
import Bio.Sequence.Parsed
import Bio.Phylogeny.Graph
import Data.IntMap ((\\), elems)
import Data.Monoid
import Data.Map (keys, adjust, insert, foldWithKey)
import Data.Vector (Vector, (++))
import File.Format.TransitionCostMatrix

data FracturedParseResult
   = FPR
   { parsedChars  :: [TreeSeqs]
   , parsedMetas  :: [Vector CharInfo]
   , parsedTrees  :: Graph
   , relatedTcm   :: Maybe TCM
   , sourceFile   :: FilePath
   } deriving (Show)
          

-- | Takes in a list of parse results and outputs 
masterUnify :: [FracturedParseResult] -> Either UnificationError Graph
masterUnify inResults = 
    let
      graphMetadata = enforceGraph $ map (foldr (mergeParsedMetadata . parsedMetas) mempty) inResults
      firstTopo = foldr (mergeParsedGraphs . parsedTrees) (Right $ parsedTrees $ head inResults) inResults
    in firstTopo

    where
      -- | Simple function to shove metadata in a tree structure
      enforceGraph :: [Vector CharInfo] -> Graph
      enforceGraph = Graph . map (\c -> mempty {characters = c})

-- | Verify that between two graphs, the taxa names are the same
checkTaxaMatch :: Graph -> Graph -> ([String], [String])
checkTaxaMatch (Graph g1) (Graph g2) = 
    let
        allNames1 = foldr1 (<>) (map nodeNames g1)
        allNames2 = foldr1 (<>) (map nodeNames g2)
    in (elems $ allNames1 \\ allNames2, elems $ allNames2 \\ allNames1)

-- | Functionality to fully merge sets of metadata
-- we make a lot of assumptions about whether the metadata agrees and assume:
-- that each item of the list CORRECTLY corresponds to a single file
-- and each element of the vector CORRECTLY identifies a single character in that file
-- if either of these assumptions are violated, this thing becomes more complicated
mergeParsedMetadata :: [Vector CharInfo] -> Vector CharInfo -> Vector CharInfo
mergeParsedMetadata = foldr (\e acc -> acc ++ e)

-- | Verify that between a graph and parsed sequences, the taxa names match
checkTaxaSeqs :: Graph -> TreeSeqs -> ([String], [String])
checkTaxaSeqs = mempty

-- | Specialized merge to join sequences to an existing graph
mergeParsedChars :: [TreeSeqs] -> Either UnificationError Graph -> Either UnificationError Graph
mergeParsedChars inSeqs carry = case carry of
  Left e -> Left e
  Right g -> addEncodeSeqs g
    where
      addEncodeSeqs :: Graph -> Either UnificationError Graph
      addEncodeSeqs accumDag@(Graph accumDags)
        | null matchCheck = 
          let outSeqs = map (foldWithKey addIn mempty) inSeqs
          in Right (Graph $ zipWith (\d s -> d {parsedSeqs = s}) accumDags outSeqs)
        | otherwise = Left (UnificationError (pure (NonMatchingTaxa (fromList $ fst matchCheck) (fromList $ snd matchCheck))))
        where
          matchCheck = checkTaxaSeqs accumDag inSeqs
          addIn k v acc = if k `elem` (keys acc) then adjust (++ pure v) k acc
                            else insert k v acc

-- | Specialized functionality to merge parsed graphs (topological structure only)
mergeParsedGraphs :: Graph -> Either UnificationError Graph -> Either UnificationError Graph
mergeParsedGraphs graph1@(Graph newGraph) carry = case carry of
  Left e -> Left e
  Right g -> matchThese g
  where
    matchThese :: Graph -> Either UnificationError Graph
    matchThese in2@(Graph accumGraph)
      | null matchCheck = 
        let notNull = filter (not . null . nodes) newGraph
        in Right $ Graph (accumGraph <> notNull)
      | otherwise = Left (UnificationError (pure (NonMatchingTaxa (fromList $ fst matchCheck) (fromList $ snd matchCheck)))) 
        where
          matchCheck = checkTaxaMatch graph1 in2

