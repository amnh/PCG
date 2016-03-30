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
import           Data.Foldable
import qualified Data.HashMap.Lazy as HM
import           Data.IntMap             (elems)
import qualified Data.IntMap       as IM 
import           Data.Key                ((!))
import           Data.List               ((\\), isPrefixOf, nub)
import           Data.Map                (foldWithKey, difference, intersectionWith)
import qualified Data.Map          as M 
import           Data.Monoid
import           Data.Vector             (Vector, (//), cons, generate, imap)
import qualified Data.Vector       as V  (replicate, foldr, (!))
import           File.Format.Conversion.Encoder
import           File.Format.TransitionCostMatrix
import           PCG.Command.Types.Read.Unification.UnificationError

import Debug.Trace

data FracturedParseResult
   = FPR
   { parsedChars  :: TreeSeqs
   , parsedMetas  :: [Vector CharInfo]
   , parsedTrees  :: Graph
   , relatedTcm   :: Maybe TCM
   , sourceFile   :: FilePath
   } deriving (Show)
          

-- | Takes in a list of parse results and outputs 
-- accumulates in metadata, then topological structure, then sequences 
-- before encoding and outputting
masterUnify :: [FracturedParseResult] -> Either UnificationError Graph
--masterUnify inResults | trace ("initial input " <> show (map parsedChars inResults)) False = undefined
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
--      enforceGraph _graph chars | trace ("enforce graph on " <> show chars) False = undefined
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
    filterEmpty = filter (null . nodes) newGraph
    matchThese :: Graph -> Either UnificationError Graph
    matchThese in2@(Graph accumGraph)
      | doesMatch = Right $ Graph (accumGraph <> filterEmpty)
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
mergeParsedMetadata inMeta = foldl (<>) mempty inMeta
                   -- TODO: Investigate above claim
                      
-- | Verify that after all the parsed sequences have been merged in, taxa names match
verifyTaxaSeqs :: Either UnificationError Graph -> Either UnificationError Graph
--verifyTaxaSeqs inGraph | trace ("verifyTaxaSeqs on " ++ show inGraph) False = undefined
verifyTaxaSeqs inGraph = eitherAction inGraph id verifySeqs
  where
    verifySeqs :: Graph -> Either UnificationError Graph
    verifySeqs (Graph g) | trace ("verify seq match " ++ show g) False = undefined
    verifySeqs (Graph g)
      | doesMatch = Right $ Graph g
      | otherwise = Left  . UnificationError . pure $ uncurry NonMatchingTaxaSeqs checkTuple
      where
        nonInternal = filter (not . isPrefixOf "HTU")
        graphNames = nub $ nonInternal . elems $ foldr ((<>) . nodeNames) mempty g
        seqNames = nonInternal $ nub $ foldr (\e acc -> acc <> HM.keys (parsedSeqs e)) mempty g
        checkTuple = (graphNames \\ seqNames, seqNames \\ graphNames)
        doesMatch = null (fst checkTuple) && null (snd checkTuple)
      

-- | Specialized merge to join sequences to an existing graph
mergeParsedChars :: [TreeSeqs] -> Either UnificationError Graph -> Either UnificationError Graph
mergeParsedChars inSeqs carry = eitherAction carry id addSeqs
    where
      addSeqs :: Graph -> Either UnificationError Graph
      addEncodeSeqs g@(Graph accumDags) | trace ("addEncodeSeqs on accumDags " <> show accumDags) False = undefined
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
          getLen = HM.foldr (\s acc -> if   length s /= acc && acc /= 0
                                       then error "Uneven sequence length" else length s) 0

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
          curNode = nodes curDAG V.! curPos
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

-- | New master unification function
masterUnify' :: [FracturedParseResult] -> Either UnificationError Graph
--masterUnify inResults | trace ("initial input " <> show (map parsedChars inResults)) False = undefined
masterUnify' inResults = encoded
  where
    -- union taxa names
    namesFromOne = foldr ((<>) . M.keys) mempty
    allNames = nub $ foldr ((<>) . namesFromOne . parsedChars) mempty inResults

    -- grab the graph with an actual topology
    topoStruc = grabTopo inResults allNames

    -- check names
    namesVerified = verifyNaming topoStruc allNames

    -- merge metadata and sequences
    seqInfo = joinSequences inResults

    -- encode into the existing nodes
    encoded = encodeGraph' seqInfo namesVerified

-- | Get a graph with a topological structure from a result
-- defaults or errors as needed
grabTopo :: [FracturedParseResult] -> [String] -> Either UnificationError Graph
grabTopo results names
  | length filtered > 1 = Left $ UnificationError $ pure $ TooManyTrees (map sourceFile filtered)
  | length filtered == 1 = Right $ parsedTrees $ head $ filtered
  | otherwise = Right $ makeEmptyNodes names
  where
    hasNodes (Graph g) = not $ null $ filter (not . null . nodes) g
    filtered = filter (hasNodes . parsedTrees) results

-- | Make empty nodes with matching names and shove into graph
-- creates a graph with node names, edges, and nodes, but nothing else
makeEmptyNodes :: [String] -> Graph
makeEmptyNodes names = Graph $ pure oneDAG
  where
    withCodes = foldr (\n acc -> IM.insert (IM.size acc) n acc) mempty names
    emptyNodes = V.replicate (IM.size withCodes) mempty
    emptyEdges = V.replicate (IM.size withCodes) mempty
    oneDAG = DAG withCodes mempty mempty emptyNodes emptyEdges 0

-- | Check that names match between dags and with seqs
verifyNaming :: Either UnificationError Graph -> [String] -> Either UnificationError Graph
verifyNaming eGraph seqNames = eGraph
  where
    nonInternal = IM.filter (not . isPrefixOf "HTU")
    namesList = map (elems . nodeNames)
    doMatch l1 l2 = if null l1 || null l2 then (mempty, mempty)
                      else ((l1 \\ l2), (l1 \\ l2))

-- | Joins the sequences of a fractured parse result
joinSequences :: [FracturedParseResult] -> (Vector CharInfo, TreeSeqs)
joinSequences =  foldl' f (mempty, mempty) . filter (not . null . parsedChars)
  where
    f :: (Vector CharInfo, TreeSeqs) -> FracturedParseResult -> (Vector CharInfo, TreeSeqs)
    f acc fpr = g acc $ foldl' g (mempty, mempty) $ zip (parsedMetas fpr) (parsedChars fpr)

    g :: (Vector CharInfo, TreeSeqs) -> (Vector CharInfo, TreeSeqs) -> (Vector CharInfo, TreeSeqs)
    g (oldMetaData, oldTreeSeqs) (nextMetaData, nextTreeSeqs) = (oldMetaData <> nextMetaData, inOnlyOld <> inBoth <> inOnlyNext)
      where
        oldPad       = generate (length  oldMetaData) (const Nothing) 
        nextPad      = generate (length nextMetaData) (const Nothing)
        inBoth       = intersectionWith (<>) oldTreeSeqs nextTreeSeqs
        inOnlyOld    = fmap (<> nextPad) $  oldTreeSeqs `difference` nextTreeSeqs
        inOnlyNext   = fmap (oldPad  <>) $ nextTreeSeqs `difference`  oldTreeSeqs

-- | New functionality to encode into a graph
encodeGraph' :: (Vector CharInfo, TreeSeqs) -> Either UnificationError Graph -> Either UnificationError Graph
encodeGraph' (metadata, taxaSeqs) inGraph = case inGraph of
  Left e -> Left e
  Right g -> Right $ encodeGraph g
  where
    encodeGraph (Graph g) = Graph $ map setNodes g
    setNodes d = d {nodes = makeNodes d}
    makeNodes d = imap (\i n -> if isLeaf n then encodeSeq i n d else n) (nodes d)
    encodeSeq pos node d = node {encoded = encodeIt (getSeq pos d) metadata}
    -- TODO: figure out why we need this stopgap check
    getName pos d = if pos `IM.member` nodeNames d then nodeNames d ! pos
                      else mempty
    getSeq pos d = if (getName pos d) `M.member` taxaSeqs then taxaSeqs ! (getName pos d)
                    else mempty
