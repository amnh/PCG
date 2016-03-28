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

import           Bio.Phylogeny.Graph
import           Bio.Sequence.Parsed
import           Bio.Phylogeny.Tree.Node
import qualified Data.HashMap.Lazy as HM
import           Data.IntMap             (elems)
import qualified Data.IntMap       as IM (assocs, filter)
import           Data.List               ((\\), isPrefixOf, nub)
import           Data.Map                (foldWithKey)
import           Data.Monoid
import           Data.Vector             (Vector, (!), (//), cons)
import qualified Data.Vector       as V  (replicate)
import           File.Format.Conversion.Encoder
import           File.Format.TransitionCostMatrix
import           PCG.Command.Types.Read.Unification.UnificationError

--import Debug.Trace

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
--masterUnify inResults | trace ("initial input " <> show (map parsedMetas inResults)) False = undefined
masterUnify inResults = 
    let
      firstTopo    = foldr (mergeParsedGraphs . parsedTrees) (Right mempty) inResults
      withMetadata = --trace ("initial graph " <> show firstTopo)
                     enforceGraph firstTopo $ (mergeParsedMetadata . parsedMetas) <$> inResults
      withSeqs     = --trace ("graph with meatadata " <> show withMetadata)
                     verifyTaxaSeqs $ foldr (mergeParsedChars . parsedChars) withMetadata inResults
      encodedRes   = --trace ("graph with seqs " <> show withSeqs)
                     encodeGraph withSeqs
    in encodedRes

    where
      -- | Simple function to shove metadata in a tree structure
      enforceGraph :: Either UnificationError Graph -> [Vector CharInfo] -> Either UnificationError Graph
      --enforceGraph graph chars | trace ("enforce graph on " <> show chars) False = undefined
      enforceGraph graph chars = eitherAction graph id (Right . Graph . shoveIt)
        where
          shoveIt (Graph dags) = if null chars then dags
                                  else zipWith (\d c -> d {characters = c}) dags chars

-- | Verify that between two graphs, the taxa names are the same
checkTaxaMatch :: Graph -> Graph -> ([String], [String])
--checkTaxaMatch (Graph g1) (Graph g2) | trace ("checking taxa match " <> show g1 <> "\n" <> show g2) False = undefined
checkTaxaMatch (Graph g1) (Graph g2) 
  | null allNames1 || null allNames2 = (mempty, mempty)
  | otherwise = (allNames1 \\ allNames2, allNames2 \\ allNames1)
    where
        allNames1 = gatherNames g1
        allNames2 = gatherNames g2
        nonInternal = IM.filter (not . isPrefixOf "HTU")
        gatherNames = elems . nonInternal . foldr ((<>) . nodeNames) mempty

-- | Specialized functionality to merge parsed graphs, simply adding the lists of dags together
mergeParsedGraphs :: Graph -> Either UnificationError Graph -> Either UnificationError Graph
mergeParsedGraphs graph1@(Graph newGraph) carry = eitherAction carry id matchThese
  where
    matchThese :: Graph -> Either UnificationError Graph
    matchThese in2@(Graph accumGraph)
      | doesMatch = Right $ Graph (accumGraph <> newGraph)
      | otherwise = Left . UnificationError . pure  $ uncurry NonMatchingTaxa matchCheck
        where
          matchCheck = checkTaxaMatch graph1 in2
          doesMatch  = null (fst matchCheck) && null (snd matchCheck)

-- | Functionality to fully merge sets of metadata
-- we make a lot of assumptions about whether the metadata agrees and assume:
-- that each item of the list CORRECTLY corresponds to a single file
-- and each element of the vector CORRECTLY identifies a single character in that file
-- if either of these assumptions are violated, this thing becomes more complicated
mergeParsedMetadata :: [Vector CharInfo] -> Vector CharInfo
--mergeParsedMetadata inMeta | trace ("merge metadata " <> show inMeta) False = undefined
mergeParsedMetadata = foldr (flip (<>)) mempty
                   -- foldl (<>) mempty <-- I think this is equivelent
                   -- TODO: Investigate above claim
                      
-- | Verify that after all the parsed sequences have been merged in, taxa names match
verifyTaxaSeqs :: Either UnificationError Graph -> Either UnificationError Graph
verifyTaxaSeqs inGraph = eitherAction inGraph id verifySeqs
  where
    verifySeqs :: Graph -> Either UnificationError Graph
    verifySeqs (Graph g)
      | doesMatch = Right $ Graph g
      | otherwise = Left  . UnificationError . pure $ uncurry NonMatchingTaxaSeqs checkTuple
      where
        nonInternal = filter (not . isPrefixOf "HTU")
        graphNames = nonInternal . elems $ foldr1 (<>) (fmap nodeNames g)
        seqNames = nub $ foldr (\e acc -> acc <> HM.keys (parsedSeqs e)) mempty g
        checkTuple = (graphNames \\ seqNames, seqNames \\ graphNames)
        doesMatch = null (fst checkTuple) && null (snd checkTuple)
      

-- | Specialized merge to join sequences to an existing graph
mergeParsedChars :: [TreeSeqs] -> Either UnificationError Graph -> Either UnificationError Graph
mergeParsedChars inSeqs carry = eitherAction carry id addSeqs
    where
      addSeqs :: Graph -> Either UnificationError Graph
      --addEncodeSeqs g@(Graph accumDags) | trace ("addEncodeSeqs on accumDags " <> show accumDags) False = undefined
      addSeqs g@(Graph accumDags)
        | null inSeqs = Right g
        | otherwise = Right (Graph $ zipWith (\d s -> d {parsedSeqs = s}) accumDags outSeqs)
        -- otherwise = Left (UnificationError (pure (NonMatchingTaxaSeqs ({-fromList $-} fst matchCheck) ({-fromList $-} snd matchCheck))))
        where
        --  matchCheck = checkTaxaSeqs g inSeqs
        --  doesMatch = null (fst matchCheck) && null (snd matchCheck)

          curSeqLen = getLen . parsedSeqs <$> accumDags
          outSeqs   = zipWith (\s l -> foldWithKey (addIn l) mempty s) inSeqs curSeqLen

          addIn :: Int -> String -> ParsedSequences -> HM.HashMap String ParsedSequences -> HM.HashMap String ParsedSequences
          addIn curLen k v acc = if k `elem` HM.keys acc
                                 then HM.adjust (<> v) k acc
                                 else HM.insert k (V.replicate curLen Nothing <> v) acc

          getLen :: HM.HashMap Identifier ParsedSequences -> Int
          getLen = HM.foldr (\s acc -> if   length s /= acc
                                       then error "Uneven sequence length"                                      else length s) 0

-- | Finally, functionality to do an encoding over a graph
encodeGraph :: Either UnificationError Graph -> Either UnificationError Graph
encodeGraph inGraph = eitherAction inGraph id (Right . onGraph)
  where
    onGraph (Graph g) = Graph $ fmap determineBuild g
    determineBuild inDAG = if null $ nodes inDAG then encodeOver inDAG buildWithSeqs 
                            else encodeOver inDAG encodeNode
    encodeOver startDAG f = foldr f startDAG (IM.assocs $ nodeNames startDAG)

    -- | Wrapper to allow encoding on a node
    -- assumes that the correct amount of nodes is already present
    encodeNode :: (Int,Identifier) -> DAG -> DAG
    --encodeNode curPos curName curDAG | trace ("trying to encode " <> show (parsedSeqs curDAG)) False = undefined
    encodeNode (curPos,curName) curDAG 
      | isLeaf curNode = curDAG { nodes = nodes curDAG // [(curPos, newNode)] }
      | otherwise = curDAG 
        where
          curNode = nodes curDAG ! curPos
          curSeqs = parsedSeqs curDAG HM.! curName
          newNode = curNode {encoded = encodeIt curSeqs (characters curDAG)}

    -- | Encodes a bunch of disconnected nodes given the sequences
    buildWithSeqs :: (Int,Identifier) -> DAG -> DAG
    buildWithSeqs (curPos,curName) curDAG = curDAG { nodes = generatedNode `cons` nodes curDAG, edges = mempty `cons` edges curDAG }
      where
          curSeqs = parsedSeqs curDAG HM.! curName
          generatedNode = mempty { code = curPos, encoded = encodeIt curSeqs (characters curDAG) }

-- | Simple helper function for partitioning either
eitherAction :: Either a b -> (a -> a) -> (b -> Either a b) -> Either a b
eitherAction inVal fun1 fun2 = case inVal of
  Left x -> Left $ fun1 x
  Right y -> fun2 y
