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
import qualified Prelude as P

import File.Format.Conversion.Encoder
import File.Format.TransitionCostMatrix
import PCG.Command.Types.Read.Unification.UnificationError

import Bio.Metadata.Class
import Bio.Phylogeny.Graph
import Bio.Phylogeny.Graph.Parsed
import Bio.Sequence.Coded
import Bio.Sequence.Parsed
import Bio.Sequence.Parsed.Class
import Bio.Phylogeny.PhyloCharacter
import Bio.Phylogeny.Tree.Node

import Control.Monad
import Data.Bits
import Data.BitVector (BitVector)
import qualified Data.HashMap.Lazy as HM
import Data.IntMap (elems)
import qualified Data.IntMap as IM (foldWithKey, assocs, filter)
import qualified Data.List as L ((\\), (++), isPrefixOf, nub)
import Data.List.NonEmpty (fromList)
import Data.Map (keys, adjust, insert, foldWithKey)
import Data.Monoid
import qualified Data.Set as S
import Data.Vector (Vector, (++), (!), (//), cons)
import qualified Data.Vector as V (zipWith, replicate, length)

import Debug.Trace

data FracturedParseResult
   = FPR
   { parsedChars  :: [TreeSeqs]
   , parsedMetas  :: [Vector CharInfo]
   , parsedTrees  :: Graph
   , relatedTcm   :: Maybe TCM
   , sourceFile   :: FilePath
   } deriving (Show)
          

-- | Takes in a list of parse results and outputs 
-- accumulates in metadata, then topological structure, then sequences 
-- before encoding and outputting
masterUnify :: [FracturedParseResult] -> Either UnificationError Graph
masterUnify inResults | trace ("initial input " P.++ show (map parsedMetas inResults)) False = undefined
masterUnify inResults = 
    let
      firstTopo = foldr (mergeParsedGraphs . parsedTrees) (Right mempty) inResults
      withMetadata = --trace ("initial graph " P.++ show firstTopo)
                    enforceGraph firstTopo $ map (mergeParsedMetadata . parsedMetas) inResults
      withSeqs = --trace ("graph with meatadata " P.++ show withMetadata)
                  verifyTaxaSeqs $ foldr (mergeParsedChars . parsedChars) withMetadata inResults
      encoded = --trace ("graph with seqs " P.++ show withSeqs)
                encodeGraph withSeqs
    in encoded

    where
      -- | Simple function to shove metadata in a tree structure
      enforceGraph :: Either UnificationError Graph -> [Vector CharInfo] -> Either UnificationError Graph
      --enforceGraph graph chars | trace ("enforce graph on " P.++ show chars) False = undefined
      enforceGraph graph chars = eitherAction graph id (Right . Graph . shoveIt)
        where
          shoveIt (Graph dags) = if null chars then dags
                                  else zipWith (\d c -> d {characters = c}) dags chars

-- | Verify that between two graphs, the taxa names are the same
checkTaxaMatch :: Graph -> Graph -> ([String], [String])
--checkTaxaMatch (Graph g1) (Graph g2) | trace ("checking taxa match " P.++ show g1 P.++ "\n" P.++ show g2) False = undefined
checkTaxaMatch (Graph g1) (Graph g2) 
  | null allNames1 || null allNames2 = (mempty, mempty)
  | otherwise = (allNames1 L.\\ allNames2, allNames2 L.\\ allNames1)
    where
        allNames1 = elems $ nonInternal $ foldr (<>) mempty (map nodeNames g1)
        allNames2 = elems $ nonInternal $ foldr (<>) mempty (map nodeNames g2)
        nonInternal = IM.filter (not . L.isPrefixOf "HTU")

-- | Specialized functionality to merge parsed graphs, simply adding the lists of dags together
mergeParsedGraphs :: Graph -> Either UnificationError Graph -> Either UnificationError Graph
mergeParsedGraphs graph1@(Graph newGraph) carry = eitherAction carry id matchThese
  where
    matchThese :: Graph -> Either UnificationError Graph
    matchThese in2@(Graph accumGraph)
      | doesMatch = Right $ Graph (accumGraph <> newGraph)
      | otherwise = Left (UnificationError (pure (NonMatchingTaxa (fst matchCheck) (snd matchCheck)))) 
        where
          matchCheck = checkTaxaMatch graph1 in2
          doesMatch = null (fst matchCheck) && null (snd matchCheck)

-- | Functionality to fully merge sets of metadata
-- we make a lot of assumptions about whether the metadata agrees and assume:
-- that each item of the list CORRECTLY corresponds to a single file
-- and each element of the vector CORRECTLY identifies a single character in that file
-- if either of these assumptions are violated, this thing becomes more complicated
mergeParsedMetadata :: [Vector CharInfo] -> Vector CharInfo
--mergeParsedMetadata inMeta | trace ("merge metadata " P.++ show inMeta) False = undefined
mergeParsedMetadata inMeta = foldr (flip (++)) mempty inMeta

-- | Verify that after all the parsed sequences have been merged in, taxa names match
verifyTaxaSeqs :: Either UnificationError Graph -> Either UnificationError Graph
verifyTaxaSeqs inGraph = eitherAction inGraph id verifySeqs
  where
    verifySeqs :: Graph -> Either UnificationError Graph
    verifySeqs (Graph g)
      | doesMatch = Right (Graph g)
      | otherwise = Left (UnificationError (pure (NonMatchingTaxaSeqs (fst checkTuple) (snd checkTuple))))
      where
        nonInternal = filter (not . L.isPrefixOf "HTU")
        graphNames = nonInternal $ elems $ foldr1 (<>) (map nodeNames g)
        seqNames = L.nub $ foldr (\g acc -> acc L.++ HM.keys (parsedSeqs g)) mempty g
        checkTuple = (graphNames L.\\ seqNames, seqNames L.\\ graphNames)
        doesMatch = null (fst checkTuple) && null (snd checkTuple)
      

-- | Specialized merge to join sequences to an existing graph
mergeParsedChars :: [TreeSeqs] -> Either UnificationError Graph -> Either UnificationError Graph
mergeParsedChars inSeqs carry = eitherAction carry id addSeqs
    where
      addSeqs :: Graph -> Either UnificationError Graph
      --addEncodeSeqs g@(Graph accumDags) | trace ("addEncodeSeqs on accumDags " P.++ show accumDags) False = undefined
      addSeqs g@(Graph accumDags)
        | null inSeqs = Right g
        | otherwise = Right (Graph $ zipWith (\d s -> d {parsedSeqs = s}) accumDags outSeqs)
        -- otherwise = Left (UnificationError (pure (NonMatchingTaxaSeqs ({-fromList $-} fst matchCheck) ({-fromList $-} snd matchCheck))))
        where
        --  matchCheck = checkTaxaSeqs g inSeqs
        --  doesMatch = null (fst matchCheck) && null (snd matchCheck)

          curSeqLen = map (getLen . parsedSeqs) accumDags
          outSeqs = zipWith (\s l -> foldWithKey (addIn l) mempty s) inSeqs curSeqLen

          addIn :: Int -> String -> ParsedSequences -> HM.HashMap String ParsedSequences -> HM.HashMap String ParsedSequences
          addIn curLen k v acc = if k `elem` (HM.keys acc) then HM.adjust (++ v) k acc
                            else HM.insert k (V.replicate curLen Nothing ++ v) acc

          getLen :: HM.HashMap Identifier ParsedSequences -> Int
          getLen inSeqs = HM.foldr (\s acc -> if (V.length s /= acc) then error "Uneven sequence length" else V.length s) 0 inSeqs

-- | Finally, functionality to do an encoding over a graph
encodeGraph :: Either UnificationError Graph -> Either UnificationError Graph
encodeGraph inGraph = eitherAction inGraph id (Right . onGraph)
  where
    onGraph (Graph g) = Graph $ map determineBuild g
    determineBuild inDAG = if null $ nodes inDAG then encodeOver inDAG buildWithSeqs 
                            else encodeOver inDAG encodeNode
    encodeOver startDAG f = foldr f startDAG (IM.assocs $ nodeNames startDAG)

    -- | Wrapper to allow encoding on a node
    -- assumes that the correct amount of nodes is already present
    encodeNode :: (Int,Identifier) -> DAG -> DAG
    --encodeNode curPos curName curDAG | trace ("trying to encode " P.++ show (parsedSeqs curDAG)) False = undefined
    encodeNode (curPos,curName) curDAG 
      | isLeaf curNode = curDAG {nodes = (nodes curDAG) // [(curPos, newNode)]}
      | otherwise = curDAG 
        where
          curNode = (nodes curDAG) ! curPos
          curSeqs = (parsedSeqs curDAG) HM.! curName
          newNode = curNode {encoded = encodeIt curSeqs (characters curDAG)}

    -- | Encodes a bunch of disconnected nodes given the sequences
    buildWithSeqs :: (Int,Identifier) -> DAG -> DAG
    buildWithSeqs (curPos,curName) curDAG = curDAG {nodes = generatedNode `cons` (nodes curDAG), edges = mempty `cons` (edges curDAG)}
      where
          curSeqs = (parsedSeqs curDAG) HM.! curName
          generatedNode = mempty {code = curPos, encoded = encodeIt curSeqs (characters curDAG)}

-- | Simple helper function for partitioning either
eitherAction :: Either a b -> (a -> a) -> (b -> Either a b) -> Either a b
eitherAction inVal fun1 fun2 = case inVal of
  Left x -> Left $ fun1 x
  Right y -> fun2 y