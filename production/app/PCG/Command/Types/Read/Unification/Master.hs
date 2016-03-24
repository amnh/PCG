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

import PCG.Command.Types.Read.Unification.UnificationError
import Bio.Phylogeny.Graph.Parsed
import Control.Monad
import Data.BitVector (BitVector)
import Data.List.NonEmpty (fromList)
import Bio.Metadata.Class
import Bio.Sequence.Coded
import Bio.Sequence.Parsed
import File.Format.Conversion.Encoder
import Bio.Sequence.Parsed.Class
import Bio.Phylogeny.Graph
import Bio.Phylogeny.Tree.Node
import Data.IntMap ((\\), elems)
import qualified Data.IntMap as IM (foldWithKey)
import Data.Monoid
import Data.Map (keys, adjust, insert, foldWithKey)
import qualified Data.HashMap.Lazy as HM
import Data.Vector (Vector, (++), (!), (//))
import qualified Data.Vector as V (zipWith, replicate, length)
import File.Format.TransitionCostMatrix
import qualified Data.Set as S
import qualified Data.List as L ((\\), (++))
import Data.Bits
import Bio.Phylogeny.PhyloCharacter

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
--masterUnify inResults | trace ("initial input " P.++ show (map parsedMetas inResults)) False = undefined
masterUnify inResults = 
    let
      --graphMetadata = Graph $ map (enforceGraph . mergeParsedMetadata . parsedMetas) inResults
      firstTopo = foldr (mergeParsedGraphs . parsedTrees) (Right mempty) inResults
      withMetadata = enforceGraph firstTopo $ map (mergeParsedMetadata . parsedMetas) inResults
      withSeqs = --trace ("graph with meatadata " P.++ show withMetadata)
                  foldr (mergeParsedChars . parsedChars) withMetadata inResults
      encoded = --trace ("graph with seqs " P.++ show withSeqs)
                encodeGraph withSeqs
    in encoded

    where
      -- | Simple function to shove metadata in a tree structure
      enforceGraph :: Either UnificationError Graph -> [Vector CharInfo] -> Either UnificationError Graph
      --enforceGraph graph chars | trace ("enforce graph on " P.++ show chars) False = undefined
      enforceGraph graph chars = eitherAction graph id (Right . Graph . shoveIt)
        where
          shoveIt (Graph dags) = zipWith (\d c -> d {characters = c}) dags chars

-- | Verify that between two graphs, the taxa names are the same
checkTaxaMatch :: Graph -> Graph -> ([String], [String])
--checkTaxaMatch (Graph g1) (Graph g2) | trace ("checking taxa match " P.++ show g1 P.++ "\n" P.++ show g2) False = undefined
checkTaxaMatch (Graph g1) (Graph g2) 
  | null g1 || null g2 = (mempty, mempty)
  | otherwise =  
    let
        allNames1 = foldr1 (<>) (map nodeNames g1)
        allNames2 = foldr1 (<>) (map nodeNames g2)
    in (elems $ allNames1 \\ allNames2, elems $ allNames2 \\ allNames1)

-- | Functionality to fully merge sets of metadata
-- we make a lot of assumptions about whether the metadata agrees and assume:
-- that each item of the list CORRECTLY corresponds to a single file
-- and each element of the vector CORRECTLY identifies a single character in that file
-- if either of these assumptions are violated, this thing becomes more complicated
mergeParsedMetadata :: [Vector CharInfo] -> Vector CharInfo
--mergeParsedMetadata inMeta | trace ("merge metadata " P.++ show inMeta) False = undefined
mergeParsedMetadata inMeta = foldr (flip (++)) mempty inMeta

-- | Verify that between a graph and parsed sequences, the taxa names match
checkTaxaSeqs :: Graph -> [TreeSeqs] -> ([String], [String])
checkTaxaSeqs (Graph g) inSeqs = 
  let
    graphNames = elems $ foldr1 (<>) (map nodeNames g)
    seqNames = foldr (\s acc -> acc L.++ keys s) mempty inSeqs
  in (graphNames L.\\ seqNames, seqNames L.\\ graphNames)

-- | Specialized merge to join sequences to an existing graph
mergeParsedChars :: [TreeSeqs] -> Either UnificationError Graph -> Either UnificationError Graph
mergeParsedChars inSeqs carry = eitherAction carry id addEncodeSeqs
    where
      addEncodeSeqs :: Graph -> Either UnificationError Graph
      addEncodeSeqs g@(Graph accumDags)
        | null (fst matchCheck) && null (snd matchCheck) = 
          let 
            curSeqLen = map (getLen . parsedSeqs) accumDags
            outSeqs = zipWith (\s l -> foldWithKey (addIn l) mempty s) inSeqs curSeqLen
          in Right (Graph $ zipWith (\d s -> d {parsedSeqs = s}) accumDags outSeqs)
        | otherwise = Left (UnificationError (pure (NonMatchingTaxaSeqs ({-fromList $-} fst matchCheck) ({-fromList $-} snd matchCheck))))
        where
          matchCheck = checkTaxaSeqs g inSeqs
          addIn :: Int -> String -> ParsedSequences -> HM.HashMap String ParsedSequences -> HM.HashMap String ParsedSequences
          addIn curLen k v acc = if k `elem` (HM.keys acc) then HM.adjust (++ v) k acc
                            else HM.insert k (V.replicate curLen Nothing ++ v) acc
          getLen :: HM.HashMap Identifier ParsedSequences -> Int
          getLen inSeqs = HM.foldr (\s acc -> if (V.length s /= acc) || acc == 0 then error "Uneven sequence length" else V.length s) 0 inSeqs

-- | Specialized functionality to merge parsed graphs, simply adding the lists of dags together
mergeParsedGraphs :: Graph -> Either UnificationError Graph -> Either UnificationError Graph
mergeParsedGraphs graph1@(Graph newGraph) carry = eitherAction carry id matchThese
  where
    matchThese :: Graph -> Either UnificationError Graph
    matchThese in2@(Graph accumGraph)
      | null (fst matchCheck) && null (snd matchCheck) = Right $ Graph (accumGraph <> newGraph)
        --let notNull = filter (not . null . nodes) newGraph 
      | otherwise = Left (UnificationError (pure (NonMatchingTaxa ({-fromList $-} fst matchCheck) ({-fromList $-} snd matchCheck)))) 
        where
          matchCheck = checkTaxaMatch graph1 in2

-- | Finally, functionality to do an encoding over a graph
encodeGraph :: Either UnificationError Graph -> Either UnificationError Graph
encodeGraph inGraph = eitherAction inGraph id (Right . onGraph)
  where
    overDAG startG = IM.foldWithKey encodeNode startG (nodeNames startG)
    onGraph (Graph g) = Graph $ map overDAG g 

    -- | Wrapper to allow encoding on a node
    encodeNode :: Int -> Identifier -> DAG -> DAG
    --encodeNode curPos curName curDAG | trace ("trying to encode " P.++ show (parsedSeqs curDAG)) False = undefined
    encodeNode curPos curName curDAG = 
      let
        curNode = (nodes curDAG) ! curPos
        curSeqs = (parsedSeqs curDAG) HM.! curName
        newNode = curNode {encoded = encodeIt curSeqs (characters curDAG)}
      in curDAG {nodes = (nodes curDAG) // [(curPos, newNode)]}

-- | Simple helper function for partitioning either
eitherAction :: Either a b -> (a -> a) -> (b -> Either a b) -> Either a b
eitherAction inVal fun1 fun2 = case inVal of
  Left x -> Left $ fun1 x
  Right y -> fun2 y
