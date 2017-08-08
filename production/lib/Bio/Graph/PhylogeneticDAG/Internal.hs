------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.PhylogeneticDAG.Internal
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

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MonoLocalBinds, MultiParamTypeClasses, ScopedTypeVariables #-}

module Bio.Graph.PhylogeneticDAG.Internal where

-- import           Bio.Graph
import           Bio.Graph.LeafSet
import           Bio.Graph.Node
import           Bio.Graph.ReferenceDAG.Internal
import           Bio.Sequence
import           Bio.Sequence.Block               (CharacterBlock)
import           Control.Applicative              (liftA2)
import           Control.Lens
import           Data.Bits
import           Data.EdgeLength
import           Data.Foldable
import           Data.GraphViz.Printing    hiding ((<>)) -- Seriously, why is this redefined?
--import           Data.Hashable
--import           Data.Hashable.Memoize
import           Data.IntSet                      (IntSet)
import qualified Data.IntSet               as IS
import           Data.Key
import           Data.List.NonEmpty               (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty        as NE
import           Data.List.Utility
import           Data.Map                         (Map)
import           Data.Maybe
import           Data.MonoTraversable
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Vector                      (Vector)
import           Prelude                   hiding (zipWith)
import           Text.XML.Custom



data PhylogeneticDAG e n u v w x y z
     = PDAG (ReferenceDAG () e (PhylogeneticNode n (CharacterSequence u v w x y z)))


data PhylogeneticDAG2 e n u v w x y z
     = PDAG2 ( ReferenceDAG
                 ( Map EdgeReference (ResolutionCache (CharacterSequence u v w x y z))
                 , Vector (Map EdgeReference (ResolutionCache (CharacterSequence u v w x y z)))
                 )
                 e
                 (PhylogeneticNode2 (CharacterSequence u v w x y z) n)
             )


type EdgeReference = (Int, Int)


instance HasLeafSet (PhylogeneticDAG2 e n u v w x y z) (LeafSet n) where

    leafSet = lens getter undefined
        where
            getter :: (PhylogeneticDAG2 e n u v w x y z) -> (LeafSet n)
            getter (PDAG2 e) = fmap nodeDecorationDatum2 $ e ^. leafSet


instance Foldable f => PrintDot (PhylogeneticDAG2 e (f String) u v w x y z) where

    unqtDot       = unqtDot . discardCharacters

    toDot         = toDot . discardCharacters

    unqtListToDot = unqtListToDot . fmap discardCharacters

    listToDot     = listToDot . fmap discardCharacters


instance ( Show e
         , Show n
         , Show u
         , Show v
         , Show w
         , Show x
         , Show y
         , Show z
         , HasBlockCost u v w x y z Word Double
         ) => Show (PhylogeneticDAG e n u v w x y z) where

    show (PDAG dag) = show dag <> "\n" <> foldMapWithKey f dag
      where
        f i (PNode n sek) = mconcat [ "Node {", show i, "}:\n\n", unlines [show n, show sek] ]


instance ( Show e
         , Show n
         , Show u
         , Show v
         , Show w
         , Show x
         , Show y
         , Show z
         , HasBlockCost u v w x y z Word Double
         ) => Show (PhylogeneticDAG2 e n u v w x y z) where

    show p@(PDAG2 dag) = unlines
        [ renderSummary p
        , foldMapWithKey f dag
        ]
      where
--        f i (PNode2 n sek) = mconcat [ "Node {", show i, "}:\n\n", unlines [show n, show sek], "\n\n" ]
        f i n = mconcat [ "Node {", show i, "}:\n\n", show n ]


instance ( ToXML u
         , ToXML v
         , ToXML w
         , ToXML x
         , ToXML y
         , ToXML z
         ) => ToXML (PhylogeneticDAG2 e n u v w x y z)  where

    toXML (PDAG2 refDag) = toXML refDag


applySoftwireResolutions :: [(ResolutionCache s, IntSet)] -> NonEmpty [ResolutionInformation s]
applySoftwireResolutions inputContexts =
    case inputContexts of
      []   -> pure []
      [x]  ->
          let y = pure <$> fst x
          -- TODO: review this logic thouroughly
          in  if   multipleParents x
              then y -- <> pure []
              else y
      x:y:_ -> pairingLogic (x,y)
  where
    multipleParents = not . isSingleton . otoList . snd
{-
    pairingLogic :: ( (ResolutionCache s), IntSet)
                    , (ResolutionCache s), IntSet)
                    )
                 -> NonEmpty [ResolutionInformation s]
-}
    pairingLogic (lhs, rhs) =
        case (multipleParents lhs, multipleParents rhs) of
          (False, False) -> pairedSet
          (False, True ) -> pairedSet <> lhsSet
          (True , False) -> pairedSet <> rhsSet
          (True , True ) -> pairedSet <> lhsSet <> rhsSet
       where
         lhsSet = pure <$> lhs'
         rhsSet = pure <$> rhs'
         lhs'   = fst lhs
         rhs'   = fst rhs
         pairedSet =
             case cartesianProduct lhs' rhs' of
               x:xs -> {- NE.fromList . ensureNoLeavesWereOmmitted $ -} x:|xs
               []   -> error errorContext -- pure [] -- This shouldn't ever happen
           where
             errorContext = unlines
                 [ "The impossible happened!"
                 , "LHS:"
                 , shownLHS
                 , "RHS:"
                 , shownRHS
                 ]
               where
                 shownLHS = unlines . toList $ show . leafSetRepresentation <$> fst lhs
                 shownRHS = unlines . toList $ show . leafSetRepresentation <$> fst rhs

         cartesianProduct xs ys =
             [ [x,y]
             | x <- toList xs
             , y <- toList ys
             , resolutionsDoNotOverlap x y
             ]
{-
           where
             xMask = foldMap1 leafSetRepresentation xs
             yMask = foldMap1 leafSetRepresentation ys
             overlapMask = xMask .&. yMask
             properOverlapInclusion x y =
               (leafSetRepresentation x .&. overlapMask) `xor` (leafSetRepresentation y .&. overlapMask) == zeroBits
-}


generateLocalResolutions :: HasBlockCost u'' v'' w'' x'' y'' z'' Word Double
                         => (u -> [u'] -> u'')
                         -> (v -> [v'] -> v'')
                         -> (w -> [w'] -> w'')
                         -> (x -> [x'] -> x'')
                         -> (y -> [y'] -> y'')
                         -> (z -> [z'] -> z'')
                         ->  ResolutionInformation (CharacterSequence u   v   w   x   y   z  )
                         -> [ResolutionInformation (CharacterSequence u'  v'  w'  x'  y'  z' )]
                         ->  ResolutionInformation (CharacterSequence u'' v'' w'' x'' y'' z'')
generateLocalResolutions f1 f2 f3 f4 f5 f6 parentalResolutionContext childResolutionContext =
                ResInfo
                { totalSubtreeCost      = newTotalCost
                , localSequenceCost     = newLocalCost
                , subtreeEdgeSet        = newSubtreeEdgeSet
                , leafSetRepresentation = newLeafSetRep
                , subtreeRepresentation = newSubtreeRep
                , characterSequence     = newCharacterSequence
                }
              where
                newTotalCost = sequenceCost newCharacterSequence

                newLocalCost = newTotalCost - sum (totalSubtreeCost <$> childResolutionContext)

                newCharacterSequence = transformation (characterSequence parentalResolutionContext) (characterSequence <$> childResolutionContext)
                newSubtreeEdgeSet    = foldMap subtreeEdgeSet childResolutionContext

                (newLeafSetRep, newSubtreeRep) =
                    case childResolutionContext of
                      []   -> (,) <$>          leafSetRepresentation <*>          subtreeRepresentation $ parentalResolutionContext
                      x:xs -> (,) <$> foldMap1 leafSetRepresentation <*> foldMap1 subtreeRepresentation $ x:|xs

                transformation pSeq cSeqs = hexZipWith f1 f2 f3 f4 f5 f6 pSeq transposition
                  where
                    transposition =
                        case cSeqs of
                          x:xs -> hexTranspose $ x:|xs
                          []   -> let c = const []
                                  in hexmap c c c c c c pSeq


localResolutionApplication :: HasBlockCost u v w x y d' Word Double
                           => (d -> [d] -> d')
                           -> NonEmpty (ResolutionInformation (CharacterSequence u v w x y d))
                           -> ResolutionCache (CharacterSequence u v w x y d)
                           -> NonEmpty (ResolutionInformation (CharacterSequence u v w x y d'))
localResolutionApplication f x y =
    liftA2 (generateLocalResolutions id2 id2 id2 id2 id2 f) mutalatedChild relativeChildResolutions
  where
    relativeChildResolutions = applySoftwireResolutions
        [ (x, IS.singleton 0)
        , (y, IS.singleton 0)
        ]
    id2 z _ = z
    mutalatedChild = pure
        ResInfo
        { totalSubtreeCost      = 0
        , localSequenceCost     = 0
        , subtreeEdgeSet        = mempty
        , leafSetRepresentation = zeroBits
        , subtreeRepresentation = singletonNewickSerialization (0 :: Word)
        , characterSequence     = characterSequence $ NE.head x
        }


pairs :: Foldable f => f a -> [(a, a)]
pairs = f . toList
  where
    f    []  = []
    f   [_]  = []
    f (x:xs) = ((\y -> (x, y)) <$> xs) <> f xs


renderSummary :: PhylogeneticDAG2 e n u v w x y z -> String
renderSummary (PDAG2 dag) = unlines
    [ show dag
    , show $ graphData dag
    ]


resolutionsDoNotOverlap :: ResolutionInformation a -> ResolutionInformation b -> Bool
resolutionsDoNotOverlap x y = leafSetRepresentation x .&. leafSetRepresentation y == zeroBits


discardCharacters :: PhylogeneticDAG2 e n u v w x y z -> ReferenceDAG () e n
discardCharacters (PDAG2 x) = defaultMetadata $ nodeDecorationDatum2 <$> x

