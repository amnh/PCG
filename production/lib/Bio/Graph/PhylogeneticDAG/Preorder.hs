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
  ( preorderSequence'
  ) where

import           Bio.Character.Decoration.Dynamic
import           Bio.Graph.Node
import           Bio.Graph.PhylogeneticDAG.Internal
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Sequence
import qualified Bio.Sequence.Block as BLK
import           Control.Arrow             ((&&&))
--import           Control.Applicative       (liftA2)
import           Control.Lens              ((^.))
import           Control.Monad.State.Lazy
import           Data.Bifunctor
--import           Data.Bits
import           Data.EdgeSet
import           Data.Foldable
--import           Data.Hashable
--import           Data.Hashable.Memoize
import           Data.IntMap               (IntMap)
import qualified Data.IntMap        as IM
import           Data.Key
import           Data.List.NonEmpty        (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe                (catMaybes)
import           Data.MonoTraversable
import           Data.Ord                  (comparing)
import           Data.Semigroup
--import           Data.Semigroup.Foldable
import           Data.Vector               (Vector)
import qualified Data.Vector        as V
import           Prelude            hiding (zip, zipWith)

import Debug.Trace
  

type BlockTopologies = NonEmpty (EdgeSet (Int, Int))


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

            -- A "sequence" of the minimum topologies that correspond to each block.
            sequenceOfBlockMinimumTopologies :: NonEmpty (EdgeSet (Int, Int))
            sequenceOfBlockMinimumTopologies = getTopologies blockMinimalResolutions
              where
                getTopologies = fmap subtreeEdgeSet

                blockMinimalResolutions = mapWithKey f $ toBlocks sequenceWLOG

                sequenceWLOG = characterSequence . NE.head $ datumResolutions

                f key _block = minimumBy (comparing extractedBlockCost) datumResolutions
                  where
                    extractedBlockCost = blockCost . (! key) . toBlocks . characterSequence


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
        getBlock = (! key) . toBlocks . characterSequence
        currentBlock = getBlock $ selectApplicableResolutions es currentResolutions
        parentBlocks =
            case second getBlock <$> parentalResolutions of
              []   -> let c = const []
                      in  BLK.hexmap c c c c c c currentBlock
              x:xs -> let f   = zip (fst <$> (x:xs))
                          val = snd <$> x:xs
                          trs = BLK.hexTranspose $ val
                      in  BLK.hexmap id id id id id f trs



selectApplicableResolutions :: EdgeSet (Int, Int) -> ResolutionCache s -> ResolutionInformation s
selectApplicableResolutions topology cache =
    case   filter (\x -> subtreeEdgeSet x `isSubsetOf` topology) $ toList cache of
      []  -> error $ unlines
                 [ "No applicable resolution found on pre-order traversal"
                 , "Input set:  " <> show topology
                 , "Local sets: " <> show (subtreeEdgeSet <$> cache)
                 ]
      [x] -> x 
      xs  -> maximumBy (comparing (length . subtreeEdgeSet)) xs


id2 x _ = x


-- |
-- Applies a traversal logic function over a 'ReferenceDAG' in a /pre-order/ manner.
--
-- The logic function takes a current node decoration,
-- a list of parent node decorations with the logic function already applied,
-- and returns the new decoration for the current node.
preorderFromRooting
  :: ( HasBlockCost u v w x y z  Word Double
     , HasTraversalFoci z TraversalFoci
     )
  => (z -> [(Word, z')] -> z')
  -> PhylogeneticDAG2 e n u v w x y z
  -> PhylogeneticDAG2 e n u v w x y z'
preorderFromRooting f (PDAG2 dag) = PDAG2 $ newDAG dag
  where
    newDAG        = RefDAG <$> const newReferences <*> rootRefs <*> defaultGraphMetadata . graphData
    dagSize       = length $ references dag
    newReferences = V.generate dagSize g
      where
        g i =
            IndexData
              <$> (applyNewDynamicCharacters (memo ! i) . nodeDecoration)
              <*> parentRefs
              <*> childRefs
              $ references dag ! i

    (edgeCostMapping, contextualNodeDatum) = graphMetadata $ graphData dag

    applyNewDynamicCharacters dynCharSeq oldNode = oldNode { resolutions = pure newResolution }
      where
        oldResolution = NE.head $ resolutions oldNode
        oldSequence   = characterSequence oldResolution
        newSequence   = fromBlocks . zipWith g dynCharSeq $ toBlocks oldSequence
        newResolution = oldResolution { characterSequence = newSequence }
        g newDynChars oldBlock = oldBlock { dynamicCharacters = newDynChars }


    -- |
    -- For each block, for each dynamic character, a vector of parent ref indicies.
--    parentVectors :: NonEmpty (Vector (Vector (Either Int (ResolutionCache (CharacterSequence u v w x y z)))))
    parentVectors = mapWithKey deriveParentVectors sequenceOfBlockMinimumTopologies
      where
        deriveParentVectors k (topo, dynchars) = mapWithKey h dynchars
          where
            h charIndex rootEdge@(lhsRootRef, rhsRootRef) = V.generate dagSize g
              where
                g i = mapping ! i
                
--                mapping :: IntMap (Maybe Int)
                mapping = lhs <> rhs
                  where
                    -- TODO: Get the appropriate resolution here!
                    lhs = IM.singleton lhsRootRef (Right (rhsRootRef, val)) <> genMap lhsRootRef
                    rhs = IM.singleton rhsRootRef (Right (lhsRootRef, val)) <> genMap rhsRootRef
                    genMap j = foldMap (\x -> IM.singleton x $ Left j) kids <> foldMap genMap kids
                      where
                        kids = catMaybes $ contains j <$> toList topo

                    val = (! charIndex) . dynamicCharacters
                          -- Get the appropriate block from the resolution that contains this character
                        . (! k) . toBlocks . characterSequence
                          -- Get the appropriate resolution based on this character's display tree toplogy
                        $ selectApplicableResolutions topo $ edgeCostMapping ! rootEdge

                    contains i (x,y)
                      | x == i    = Just y
                      | y == i    = Just x
                      | otherwise = Nothing


    -- A "sequence" of the minimum topologies that correspond to each block.
    sequenceOfBlockMinimumTopologies :: NonEmpty (EdgeSet (Int, Int), Vector (Int, Int))
    sequenceOfBlockMinimumTopologies = blockMinimalResolutions
      where
        blockMinimalResolutions = mapWithKey f $ toBlocks sequenceWLOG

        sequenceWLOG = characterSequence $ NE.head datumResolutions

        datumResolutions = resolutions . nodeDecoration . (references dag !) . NE.head $ rootRefs dag

        f key _block = (subtreeEdgeSet &&& grabTraversalFoci)
                     $ minimumBy (comparing extractedBlockCost) datumResolutions
          where
            getBlock           = (! key) . toBlocks . characterSequence
            extractedBlockCost = blockCost . getBlock
--            grabTraversalFoci :: HasTraversalFoci z TraversalFoci => ResolutionInformation (CharacterSequence u v w x y z) -> Vector (Int, Int)
            grabTraversalFoci  = fmap (fst . NE.head . (^. traversalFoci)) . dynamicCharacters . getBlock
                                   
      
--    memo :: Vector (NonEmpty (Vector z'))
    memo = V.generate dagSize generateDatum
      where
        generateDatum i = undefined
          where
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

--          parentCharSeqOnlyDynChars :: 
            parentCharSeqOnlyDynChars = zipWithKey g parentVectors $ fst <$> sequenceOfBlockMinimumTopologies
              where
                g k v topology = foldMapWithKey h v
                  where
                    h j x = [(0,dec)]
                      where
                        dec = 
                            case x ! i of
                              Right (_, y) -> f y []
                              Left  p      -> (! j) . (! k) $ memo ! p

            
            childCharSeqOnlyDynChars = undefined
              where
                -- FoldMap is a bit inefficient with Vectors here, worry about it later.
                g k v topology = foldMapWithKey h v
                  where
                          -- Get this character from the block
                    h j x = (! j) . dynamicCharacters
                          -- Get the appropriate block from the resolution that contains this character
                          . (! k) . toBlocks . characterSequence
                          -- Get the appropriate resolution based on this character's display tree toplogy
                          $ selectApplicableResolutions topology resolutions
                      where
                        resolutions = (contextualNodeDatum ! i) ! (i, p)
                          where
                            p = case x ! i of
                                  Right (p,_) -> p
                                  -- error "Next up Batman vs The RTS!\nReady?\nFIGHT!\nPOW! BLARM! THRAP!\nBatman wins!"
                                  Left  p -> p 
            
            
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

