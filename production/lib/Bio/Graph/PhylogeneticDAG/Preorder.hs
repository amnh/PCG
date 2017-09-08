------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.Preorder
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

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Bio.Graph.PhylogeneticDAG.Preorder
  ( preorderFromRooting
  , preorderSequence'
  ) where

import           Bio.Character.Decoration.Dynamic
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Sequence
import qualified Bio.Sequence.Block as BLK
import           Control.Arrow             ((&&&))
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Foldable
--import           Data.IntMap               (IntMap)
import qualified Data.IntMap        as IM
import qualified Data.IntSet        as IS
import           Data.Key
import           Data.List.NonEmpty        (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty as NE
import           Data.Map                  (Map)
--import qualified Data.Map           as M
import           Data.Maybe
import           Data.MonoTraversable
import           Data.Ord                  (comparing)
import           Data.Semigroup
import           Data.TopologyRepresentation
import           Data.Vector               (Vector)
import qualified Data.Vector        as V
import           Data.Vector.Instances     ()
import           Prelude            hiding (lookup, zip, zipWith)

--import Debug.Trace
  

type BlockTopologies = NonEmpty TraversalTopology


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration,
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
preorderSequence' :: (-- Eq z, Eq z', Hashable z, Hashable z'
                       HasBlockCost u  v  w  x  y  z  Word Double
--                     , HasBlockCost u' v' w' x' y' z' Word Double
                     )
                  => (u -> [(Word, u')] -> u')
                  -> (v -> [(Word, v')] -> v')
                  -> (w -> [(Word, w')] -> w')
                  -> (x -> [(Word, x')] -> x')
                  -> (y -> [(Word, y')] -> y')
                  -> (z -> [(Word, z')] -> z')
                  -> PhylogeneticDAG2 e n u  v  w  x  y  z
                  -> PhylogeneticDAG2 e n u' v' w' x' y' z'
preorderSequence' f1 f2 f3 f4 f5 f6 (PDAG2 dag) = PDAG2 $ newDAG dag
  where
    newDAG        = RefDAG <$> const newReferences <*> rootRefs <*> defaultGraphMetadata . graphData
    dagSize       = length $ references dag
    newReferences = V.generate dagSize g
      where
        g i = IndexData <$> const (snd $ memo ! i) <*> parentRefs <*> childRefs $ references dag ! i

    -- A "sequence" of the minimum topologies that correspond to each block.
    sequenceOfBlockMinimumTopologies :: BlockTopologies
    sequenceOfBlockMinimumTopologies = getTopologies blockMinimalResolutions
      where
        getTopologies = fmap topologyRepresentation

        blockMinimalResolutions = mapWithKey f $ toBlocks sequenceWLOG

        sequenceWLOG = characterSequence $ NE.head rootResolutions

        f key _block = minimumBy (comparing extractedBlockCost)
--                     $ (\x -> trace (show $ extractedBlockCost <$> toList x) x)
                       rootResolutions
          where
            extractedBlockCost = blockCost . (! key) . toBlocks . characterSequence

        rootResolutions = -- (\x -> trace ("Root resolutions: " <> show (length x)) x) $
                          resolutions . nodeDecoration $ references dag ! rootWLOG

        rootWLOG = NE.head $ rootRefs dag


--    memo :: Vector (BlockTopologies, PhylogeneticNode2 n (CharacterSequence u' v' w' x' y' z'))
    memo = V.generate dagSize g
      where
        g i = (inheritedToplogies,
            PNode2
            { resolutions          = newResolution
            , nodeDecorationDatum2 = nodeDecorationDatum2 $ nodeDecoration node
            }
            )
          where
            (inheritedToplogies, newResolution)
              | i `elem` rootRefs dag =
                let newSequence = computeOnApplicableResolution f1 f2 f3 f4 f5 f6 sequenceOfBlockMinimumTopologies datumResolutions []
                in (sequenceOfBlockMinimumTopologies, mockResInfo datumResolutions newSequence)
              | otherwise             =
                let newSequence = computeOnApplicableResolution f1 f2 f3 f4 f5 f6 parentalToplogies datumResolutions parentalResolutions
                in  (parentalToplogies, mockResInfo datumResolutions newSequence)

--            completeCoverage = (completeLeafSet ==) . (completeLeafSet .&.) . leafSetRepresentation
--            localResolutions = liftA2 (generateLocalResolutions f1 f2 f3 f4 f5 f6') datumResolutions childResolutions
--            completeLeafSet  = complement $ wlog `xor`wlog
--              where
--                wlog = leafSetRepresentation $ NE.head localResolutions
                
            datumResolutions = resolutions $ nodeDecoration node
            
--            childResolutions :: NonEmpty [a]
--            childResolutions = applySoftwireResolutions $ extractResolutionContext <$> childIndices
--            extractResolutionContext = getResolutions &&& parentRefs . (references dag !)
--            getResolutions j = fmap (addEdgeToEdgeSet (i,j)) . resolutions $ memo ! j


            node            = references dag ! i
--            childIndices    = IM.keys $ childRefs node
            parentIndices   = otoList $ parentRefs node
            -- In sparsely connected graphs (like ours) this will be effectively constant.
            childPosition j = toEnum . length . takeWhile (/=i) . IM.keys . childRefs $ references dag ! j
            parentContexts  = (\x -> second (const (childPosition x) &&& NE.head . resolutions) $ memo ! x) <$> parentIndices
            parentalResolutions = snd <$> parentContexts
            parentalToplogies   = fst $ head parentContexts


mockResInfo :: ResolutionCache s -> s' -> ResolutionCache s'
mockResInfo currentResolutions newSequence =
    -- Default the ResolutionInformation valus, insert the preorder sequence result
    pure .
      (ResInfo
        <$> totalSubtreeCost
        <*> localSequenceCost
        <*> leafSetRepresentation
        <*> subtreeRepresentation
        <*> subtreeEdgeSet
        <*> topologyRepresentation
        <*> const newSequence
      ) $ NE.head currentResolutions


computeOnApplicableResolution
  :: (u -> [(Word, u')] -> u')
  -> (v -> [(Word, v')] -> v')
  -> (w -> [(Word, w')] -> w')
  -> (x -> [(Word, x')] -> x')
  -> (y -> [(Word, y')] -> y')
  -> (z -> [(Word, z')] -> z')
  -> BlockTopologies
  -> ResolutionCache (CharacterSequence u v w x y z)
  -> [(Word, ResolutionInformation (CharacterSequence u' v' w' x' y' z'))]
  -> CharacterSequence u' v' w' x' y' z'
computeOnApplicableResolution f1 f2 f3 f4 f5 f6 topologies currentResolutions parentalResolutions =
    fromBlocks $ mapWithKey g topologies
  where
    g key es = BLK.hexZipWith f1 f2 f3 f4 f5 f6 currentBlock parentBlocks
      where
     -- We can't use this below because the monomorphism restriction is quite dumb at deduction.
     -- getBlock = (! key) . toBlocks . characterSequence
        currentBlock = ((! key) . toBlocks . characterSequence) $ selectApplicableResolutions es currentResolutions
        parentBlocks =
            case second ((! key) . toBlocks . characterSequence) <$> parentalResolutions of
              []   -> let c = const []
                      in  BLK.hexmap c c c c c c currentBlock
              x:xs -> let
                  -- We can't use this below because the monomorphism restriction is quite dumb at deduction.
                  --      f   = zip (fst <$> (x:xs))
                          val = snd <$> x:xs
                          trs = BLK.hexTranspose val
                      in  BLK.hexmap
                            (zip (fst <$> (x:xs)))
                            (zip (fst <$> (x:xs)))
                            (zip (fst <$> (x:xs)))
                            (zip (fst <$> (x:xs)))
                            (zip (fst <$> (x:xs)))
                            (zip (fst <$> (x:xs)))
                              trs



selectApplicableResolutions :: TraversalTopology -> ResolutionCache s -> ResolutionInformation s
selectApplicableResolutions topology cache =
    case filter (\x -> topologyRepresentation x `isCompatableSubtopologyOf` topology) $ toList cache of
      []  -> error $ unlines
                 [ "No applicable resolution found on pre-order traversal"
                 , "Input set:  " <> show topology
                 , "Local sets: " <> show (subtreeEdgeSet <$> cache)
                 ]
      [x] -> x 
      xs  -> maximumBy (comparing (length . subtreeEdgeSet)) xs


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration,
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
preorderFromRooting
  :: ( HasBlockCost u  v  w  x  y  z   Word Double
     , HasBlockCost u' v' w' x' y' z'  Word Double
     , HasTraversalFoci z  (Maybe TraversalFoci)
     , HasTraversalFoci z' (Maybe TraversalFoci)
--     , Show z
     )
  => (z -> [(Word, z')] -> z')
  -> Map EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
  -> Vector (Map EdgeReference (ResolutionCache (CharacterSequence u v w x y z)))
  -> PhylogeneticDAG2 e' n' u' v' w' x' y' z
  -> PhylogeneticDAG2 e' n' u' v' w' x' y' z'
preorderFromRooting f edgeCostMapping contextualNodeDatum (PDAG2 dag) = PDAG2 $ newDAG dag
  where
    newDAG        = RefDAG <$> const newReferences <*> rootRefs <*> defaultGraphMetadata . graphData
    dagSize       = length $ references dag
    roots         = rootRefs dag
    newReferences = V.generate dagSize g
      where
        g i =
            IndexData
              <$> (applyNewDynamicCharacters (memo ! i) . nodeDecoration)
              <*> parentRefs
              <*> childRefs
              $ references dag ! i

--    (edgeCostMapping, contextualNodeDatum) = graphMetadata $ graphData dag

    applyNewDynamicCharacters dynCharSeq oldNode = oldNode { resolutions = pure newResolution }
      where
        oldResolution = NE.head $ resolutions oldNode
        oldSequence   = characterSequence oldResolution
        newSequence   = fromBlocks . zipWith g dynCharSeq $ toBlocks oldSequence
        newResolution = oldResolution { characterSequence = newSequence }
        g newDynChars oldBlock = oldBlock { dynamicCharacters = newDynChars }


    -- |
    -- For each block, for each dynamic character, a vector of parent ref indicies.
--    parentVectors :: NonEmpty (Vector (Vector (Either Int (Int, ResolutionCache (CharacterSequence u v w x y z)))))
    parentVectors = {-
                  trace "after force !!"
                  . (\x -> trace ("before force !!" <> show (fmap (fmap (fmap (fmap (fmap (const ()))))) x)) x
                    )
                  -}
                    mapWithKey deriveParentVectors sequenceOfBlockMinimumTopologies
      where
        rootEdges    = toList $ undirectedRootEdgeSet   dag
        treeEdges    = toList $ referenceTreeEdgeSet    dag
        deriveParentVectors k (topo, dynchars) = mapWithKey h dynchars
          where
            h charIndex rootEdge@(lhsRootRef, rhsRootRef) = V.generate dagSize g
              where
--                g i | trace (unwords [show i, "/", show $ length dag, show rootEdge, show $ IM.keys parentalMapping]) False = undefined
                g i = parentalMapping ! i
                
                parentalMapping = lhs <> rhs
                  where
                    -- TODO: Get the appropriate resolution here!
                    lhs = IM.singleton lhsRootRef (Right (rhsRootRef, val)) <> genMap (IS.singleton rhsRootRef) lhsRootRef
                    rhs = IM.singleton rhsRootRef (Right (lhsRootRef, val)) <> genMap (IS.singleton lhsRootRef) rhsRootRef
--                    genMap _  j | trace (show j) False = undefined
                    genMap is j = foldMap (\x -> IM.singleton x $ Left j) kids <> foldMap (genMap (IS.insert j is)) kids
                      where
                        kids  = catMaybes $ nextEdges j is <$> topoEdges
                        topoEdges = toList topo <> treeEdges <> rootEdges

                    val = (! charIndex) . dynamicCharacters
                          -- Get the appropriate block from the resolution that contains this character
                        . (! k) . toBlocks . characterSequence
                          -- Get the appropriate resolution based on this character's display tree toplogy
                        . selectApplicableResolutions topo $ edgeCostMapping ! rootEdge

                    nextEdges i is (x,y)
                      | x == i && isValid x = Just y
                      | y == i && isValid y = Just x
                      | otherwise           = Nothing
                      where
                        isValid j = j `onotElem` is && j `notElem` roots


    -- A "sequence" of the minimum topologies that correspond to each block.
    sequenceOfBlockMinimumTopologies :: NonEmpty (TraversalTopology, Vector (Int, Int))
    sequenceOfBlockMinimumTopologies = --trace "after force" $ force (trace "before force" blockMinimalResolutions)
        blockMinimalResolutions
      where
        blockMinimalResolutions = mapWithKey g $ toBlocks sequenceWLOG

        sequenceWLOG = characterSequence $ NE.head datumResolutions

        datumResolutions = resolutions . nodeDecoration $ references dag ! rootWLOG

        g key _block = (topologyRepresentation &&& grabTraversalFoci)
                     $ minimumBy (comparing extractedBlockCost) datumResolutions
          where
            getBlock           = (! key) . toBlocks . characterSequence
            extractedBlockCost = blockCost . getBlock
--            grabTraversalFoci :: HasTraversalFoci z TraversalFoci => ResolutionInformation (CharacterSequence u v w x y z) -> Vector (Int, Int)
            grabTraversalFoci  = fmap (fst . NE.head . fromJust . (^. traversalFoci)) . dynamicCharacters . getBlock
                                   

    rootWLOG = NE.head $ rootRefs dag

--    fociWLOG = sequenceOfBlockMinimumTopologies

--    applyMetadata :: NonEmpty (Vector z') -> NonEmpty (Vector z')
    applyMetadata = zipWith g sequenceOfBlockMinimumTopologies 
      where
        g (topo, foci) = zipWith h foci
          where
            h focus dec = dec & traversalFoci .~ (Just $ (focus,topo):|[])

        
    
      
--    memo :: Vector (NonEmpty (Vector z'))
    memo = V.generate dagSize generateDatum
      where
        generateDatum i
          | i `notElem` rootRefs dag = applyMetadata $ zipWith (zipWith f) childCharSeqOnlyDynChars parentCharSeqOnlyDynChars
          | otherwise                = applyMetadata $ memo ! adjacentIndex
          where
            adjacentIndex = head . IM.keys . childRefs $ references dag ! i
{-
            (inheritedToplogies, newResolution)
              | i `elem` rootRefs dag =
                let newSequence = computeOnApplicableResolution id2 id2 id2 id2 id2 f sequenceOfBlockMinimumTopologies datumResolutions []
                in (sequenceOfBlockMinimumTopologies, mockResInfo datumResolutions newSequence)
              | otherwise             =
                let newSequence = computeOnApplicableResolution id2 id2 id2 id2 id2 f parentalToplogies datumResolutions parentalResolutions
                in  (parentalToplogies, mockResInfo datumResolutions newSequence)

--            completeCoverage = (completeLeafSet ==) . (completeLeafSet .&.) . leafSetRepresentation
--            localResolutions = liftA2 (generateLocalResolutions f1 f2 f3 f4 f5 f6') datumResolutions childResolutions
--            completeLeafSet  = complement $ wlog `xor` wlog
--              where
--                wlog = leafSetRepresentation $ NE.head localResolutions
-}

--            parentCharSeqOnlyDynChars :: NonEmpty (Vector [a])
            parentCharSeqOnlyDynChars = mapWithKey g parentVectors
              where
                g k = mapWithKey h
                  where
                    h j x = [(0,dec)] -- Aways labeled as the first child (0) of the parent is technically incorrect. Probably won't matter, probably.
                      where
                        dec = 
                            case x ! i of
                              Right (_, y) -> f y []
                              Left  p      -> if i == p
                                              then error $ "Recursive memoizeation for " <> show i
                                              else (! j) . (! k) $ memo ! p

            
--          childCharSeqOnlyDynChars   :: NonEmpty (Vector a)
            childCharSeqOnlyDynChars = zipWithKey g parentVectors $ fst <$> sequenceOfBlockMinimumTopologies
              where
                -- FoldMap is a bit inefficient with Vectors here, worry about it later.
                g k v topology = mapWithKey h v
                  where
                          -- Get this character from the block
                    h j x = (! j) . dynamicCharacters
                          -- Get the appropriate block from the resolution that contains this character
                          . (! k) . toBlocks . characterSequence
                          -- Get the appropriate resolution based on this character's display tree toplogy
                          $ selectApplicableResolutions topology directedResolutions
                      where
                        directedResolutions = --(contextualNodeDatum ! i) ! (trace (unwords ["\nkey:",show i,"sub-key:",show (p,i),"\nmapping sub-keys:",show (M.keys $ contextualNodeDatum ! i)])) (p,i)
                                               (contextualNodeDatum .!>. i) .!>. (p,i)
{-                          
                            case i `lookup` contextualNodeDatum of
                              Nothing -> error $ "Couldn't find: " <> show i
                              Just z  ->
                                case (i, p) `lookup` z of
                                  Nothing -> error $ "Couldn't find: " <> show (i, p) <> " in: " <> show (M.keys z)
                                  Just a  -> a
-}
                        p = case x ! i of
                              Right (n,_) -> n
                              Left  n     -> n
            
            
--            childResolutions :: NonEmpty [a]
--            childResolutions = applySoftwireResolutions $ extractResolutionContext <$> childIndices
--            extractResolutionContext = getResolutions &&& parentRefs . (references dag !)
--            getResolutions j = fmap (addEdgeToEdgeSet (i,j)) . resolutions $ memo ! j

{-
            node            = references dag ! i
--            childIndices    = IM.keys $ childRefs node
            parentIndices   = otoList $ parentRefs node
            -- In sparsely connected graphs (like ours) this will be effectively constant.
            childPosition j = toEnum . length . takeWhile (/=i) . IM.keys . childRefs $ references dag ! j
            parentContexts  = (\x -> second (const (childPosition x) &&& NE.head . resolutions) $ memo ! x) <$> parentIndices
            parentalResolutions = snd <$> parentContexts
            parentalToplogies   = fst $ head parentContexts
-}


(.!>.) :: (Lookup f, Show (Key f)) => f a -> Key f -> a
(.!>.) s k = fromMaybe (error $ "Could not index: " <> show k) $ k `lookup` s
