-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Forest.Parsed
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Typeclass for a parsed forest so that it can convert into an internal forest.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Bio.PhyloGraph.Forest.Parsed where


import           Bio.PhyloGraphPrime.Forest
import           Bio.PhyloGraphPrime.ReferenceDAG
-- import           Bio.PhyloGraphPrime.ZipperDAG
import           Control.Monad                            (join)
import           Data.Foldable
import           Data.IntMap                              (IntMap)
import qualified Data.IntMap                       as IM
import           Data.Key
import           Data.List.NonEmpty                       (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty                as NE
import           Data.Map                                 (Map)
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                          as Set
import           File.Format.Fasta
import           File.Format.Fastc                 hiding (Identifier)
import           File.Format.Newick
import           File.Format.Nexus                 hiding (TaxonSequenceMap)
import           File.Format.TNT
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot.Parser
import qualified File.Format.VertexEdgeRoot.Parser as VER
import           Prelude                           hiding (lookup)


-- type ParserTree   = ZipperNode (Maybe Double) (Maybe String)

-- |
-- The type of possibly present decorations on a tree from a parsed file.
type ParserTree   = ReferenceDAG (Maybe Double) (Maybe String)


-- |
-- The parser coalesced type, representing a possibly present forest.
type ParserForest = Maybe (PhylogeneticForest ParserTree)


-- |
-- The parser coalesced type, representing a possibly present forest.
type ParserForestSet = Maybe (NonEmpty (PhylogeneticForest ParserTree))


-- |
-- An internal type for representing a node with a unique numeric identifier.
data NewickEnum   = NE !Int (Maybe String) (Maybe Double) [NewickEnum] 
   

-- | Represents a parser result type which can have a possibly empty forest
--   extracted from it.
class ParsedForest a where
    unifyGraph :: a -> ParserForestSet


-- | (✔)
instance ParsedForest FastaParseResult where
    unifyGraph = const Nothing


-- | (✔)
instance ParsedForest FastcParseResult where
    unifyGraph = const Nothing


-- | (✔)
instance ParsedForest TaxonSequenceMap where
    unifyGraph = const Nothing


-- | (✔)
instance ParsedForest TCM where
    unifyGraph = const Nothing


-- | (✔)
instance ParsedForest Nexus where
    unifyGraph (Nexus _ forest) = forest
                              >>= unifyGraph . (pure:: a -> NonEmpty a)


-- | (✔)
instance ParsedForest (NonEmpty NewickForest) where
    unifyGraph = Just . fmap (PhylogeneticForest . fmap (coerceTree . relationMap . enumerate))
      where

        -- Apply generating function by indexing adjacentcy matrix.
        coerceTree mapping = unfoldDAG (mapping !) 0

        -- We assign a unique index to each node by converting the node to a NewickEnum type.
        enumerate :: NewickNode -> NewickEnum
        enumerate = (\(_,_,x) -> x) . f mempty 0
          where
            f :: Map String NewickEnum -> Int -> NewickNode -> (Map String NewickEnum, Int, NewickEnum)
            f seen n node =
                case newickLabel node >>= (`lookup` seen) of
                  Just x  -> (seen, n, x)
                  Nothing ->
                    case descendants node of
                      [] -> let enumed = NE n (newickLabel node) (branchLength node) []
                                seen'  =
                                  case newickLabel node of
                                    Nothing -> seen
                                    Just x  -> seen <> Map.singleton x enumed
                            in  (seen', n + 1, enumed)
                      xs -> let recursiveResult = NE.scanr (\e (x,y,_) -> f x y e) (seen, n+1, undefined) xs
                                (seen', n', _)  = NE.head recursiveResult
                                childEnumed     = (\(_,_,x) -> x) <$> NE.init recursiveResult
                                enumed          = NE n (newickLabel node) (branchLength node) childEnumed
                                seen''          =
                                  case newickLabel node of
                                    Nothing -> seen'
                                    Just x  -> seen' <> Map.singleton x enumed
                            in  (seen'', n', enumed)

        -- We use the unique indicies from the 'enumerate' step to build a local connectivity map.
        relationMap :: NewickEnum -> IntMap ([(Maybe Double, Int)], Maybe String, [(Maybe Double, Int)])
        relationMap root = subCall Nothing root mempty
          where
            subCall :: Maybe Int
                    -> NewickEnum
                    -> IntMap ([(Maybe Double, Int)], Maybe String, [(Maybe Double, Int)])
                    -> IntMap ([(Maybe Double, Int)], Maybe String, [(Maybe Double, Int)])
            subCall parentMay (NE ref labelMay costMay children) prevMap =
                case ref `lookup` prevMap of
                  Just (xs, datum, ys) -> IM.insert ref ((costMay, fromJust parentMay):xs, datum, ys) prevMap
                  Nothing              ->
                    let parentRefs = 
                          case parentMay of
                            Nothing -> []
                            Just x  -> [(costMay,x)]
                        currMap    = IM.insert ref (parentRefs, labelMay, f <$> children) prevMap
                    in  foldr (subCall (Just ref)) currMap children
              where
                f (NE x _ y _) = (y,x)
              

-- | (✔)
instance ParsedForest TntResult where
    unifyGraph input = fmap pure $
        case input of
          Left                forest  -> toPhylogeneticForest getTNTName <$> Just     forest
          Right (WithTaxa _ _ forest) -> toPhylogeneticForest fst        <$> nonEmpty forest
      where

        -- | Propper fmapping over Maybes and NonEmptys
        toPhylogeneticForest f = PhylogeneticForest . fmap (coerceTree . enumerate f)

        -- | Apply the generating function referencing the relational mapping.
        coerceTree mapping = unfoldDAG f 0
          where
            f i = (g $ toList parentMay, datum, g childRefs)
              where
                (parentMay, datum, childRefs) = mapping ! i
                g = fmap (\j -> (Nothing, j))

        -- | We assign a unique index to each node and creating an adjcentcy matrix.
        enumerate :: (n -> String) -> LeafyTree n -> IntMap (Maybe Int, Maybe String, [Int])
        enumerate toLabel = (\(_,_,x) -> x) . f Nothing 0
          where
            f parentRef n node =
                case node of
                  Leaf   x  -> (n, n + 1, IM.singleton n (parentRef, Just $ toLabel x, []))
                  Branch xs -> 
                    let recursiveResult = NE.scanr (\e (_,x,_) -> f (Just n) x e) (undefined, n + 1, undefined) xs
                        (_, counter, _) = NE.head recursiveResult
                        childrenRefs    = (\(x,_,_) -> x) <$> NE.init recursiveResult
                        subTreeMapping  = foldMap (\(_,_,x) -> x) $ NE.init recursiveResult
                        selfMapping     = IM.singleton n (parentRef, Nothing, childrenRefs)
                    in (n, counter, selfMapping <> subTreeMapping)

        -- | Conversion function for NodeType to string
        getTNTName :: NodeType -> String
        getTNTName node =
            case node of 
              Index  i -> show i
              Name   n -> n
              Prefix s -> s


{- -}
-- | (✔)
instance ParsedForest VER.VertexEdgeRoot where
    unifyGraph (VER vs es rs) = Just . pure . PhylogeneticForest . fmap convertToDAG . NE.fromList $ toList disconnectedRoots
      where

        childMapping = foldMap f vs
          where
            f v = Map.singleton v $ foldMap g es
              where
                g e
                  | edgeOrigin e == v = Set.singleton (edgeLength e, edgeTarget e)
                  | otherwise         = mempty

        parentMapping = foldMap f vs
          where
            f v = Map.singleton v $ foldMap g es
              where
                g e
                  | edgeTarget e == v = Set.singleton (edgeLength e, edgeOrigin e)
                  | otherwise         = mempty

        -- |
        -- We collect only disconnected roots so that we don't generate duplicate
        -- trees from connected roots.
        disconnectedRoots = foldl' f rs rs
          where
            f remainingRoots r
              | r `notElem` remainingRoots = remainingRoots
              | otherwise                  = remainingRoots `Set.difference` connectedRoots
              where
                connectedRoots = g mempty r
                g seen node
                  | node `elem` remainingRoots = Set.singleton node
                  | node `elem` seen           = mempty
                  | otherwise                  = foldMap (g seen') children
                  where
                    seen' = seen 
                    children = Set.mapMonotonic snd (childMapping ! node) `Set.difference` seen

        convertToDAG = unfoldDAG f 
          where
            f label = (pValues, Just label, cValues)
              where
                pValues =
                    case label `lookup` parentMapping of
                       Nothing -> []
                       Just xs -> toList xs
                cValues =
                    case label `lookup` childMapping of
                       Nothing -> []
                       Just xs -> toList xs
        
{- -}

{-
-- | Convert the referential forests defined by sets of verticies, edges, and
--   roots into a forest of topological tree structure.
convertVerToNewick :: VertexEdgeRoot -> Forest NewickNode
convertVerToNewick (VER _ e r) = buildNewickTree Nothing <$> toList r
  where
    buildNewickTree :: Maybe Double -> VertexLabel -> NewickNode
    buildNewickTree c n = fromJust $ newickNode kids (Just n) c
      where
        kids = fmap (uncurry buildNewickTree) . catMaybes $ kidMay n <$> toList e
        kidMay vertex edge = do
          other <- VER.connectedVertex vertex edge
          pure (VER.edgeLength edge, other)

-- | Convert the topological 'LeafyTree' structure into a forest of topological
--   tree structures with nodes that hold additional information.
convertTntToNewick :: (n -> String) -> LeafyTree n -> NewickNode
convertTntToNewick f (Leaf   x ) = fromJust $ newickNode [] (Just $ f x) Nothing -- Scary use of fromJust?
convertTntToNewick f (Branch xs) = fromJust $ newickNode (convertTntToNewick f <$> xs) Nothing Nothing

-- | Conversion function for NodeType to string
getTNTName :: NodeType -> String
getTNTName node = case node of 
    (Index i) -> show i
    (Name n) -> n
    (Prefix s) -> s
-}


