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
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}

module Bio.PhyloGraph.Forest.Parsed where

import           Bio.PhyloGraphPrime.Forest
import           Bio.PhyloGraphPrime.ZipperDAG

-- import           Bio.PhyloGraph.Forest.Internal

import           Data.Foldable
import           Data.Tree
import           Data.IntMap                              (IntMap)
import qualified Data.IntMap                       as IM
import           Data.Key
import           Data.List.NonEmpty                       (nonEmpty)
import qualified Data.List.NonEmpty                as NE
import           Data.Map                                 (Map)
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.Monoid
import           File.Format.Fasta
import           File.Format.Fastc                 hiding (Identifier)
import           File.Format.Newick
import           File.Format.Nexus                 hiding (TaxonSequenceMap)
import           File.Format.TNT
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot.Parser        (VertexEdgeRoot(..),VertexLabel)
import qualified File.Format.VertexEdgeRoot.Parser as VER
import Prelude hiding (lookup, scanr)


type ParserTree   = ZipperNode (Maybe Double) (Maybe String)

type ParserForest = Maybe (PhylogeneticForest ParserTree)

data NewickEnum = NE !Int (Maybe String) (Maybe Double) [NewickEnum] 
   

-- | Represents a parser result type which can have a possibly empty forest
--   extracted from it.
class ParsedForest a where
    unifyGraph :: a -> ParserForest


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
instance ParsedForest NewickForest where
    unifyGraph = fmap (PhylogeneticForest . fmap (coerceTree . relationMap . enumerate)) . nonEmpty
      where
        coerceTree mapping = unfoldDAG (mapping !) 0

        -- We assign a unique index to each node by converting the node to a NewickEnum type.
        enumerate :: NewickNode -> NewickEnum
        enumerate = (\(_,_,x) -> x) . f mempty 0
          where
            f :: Map String NewickEnum -> Int -> NewickNode -> (Map String NewickEnum, Int, NewickEnum)
            f seen n node =
                case (newickLabel node) >>= (`lookup` seen) of
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
                            in  (seen'', n + 1, enumed)

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
    unifyGraph input =
        case input of
          Left                forest  -> PhylogeneticForest . fmap (coerceTree . enumerate getTNTName) <$> Just     forest
          Right (WithTaxa _ _ forest) -> PhylogeneticForest . fmap (coerceTree . enumerate fst       ) <$> nonEmpty forest
      where

        -- | Apply the generating function referencing the relational mapping.
        coerceTree mapping = unfoldDAG f 0
          where
            f i = (g $ maybeToList parentMay, datum, g childRefs)
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


{-
-- | (✔)
instance ParsedForest VER.VertexEdgeRoot where
    unifyGraph ver = fmap (convertToDAG) $ roots ver
      where
        convertToDAG rootLabel = 
          where
            children 
-}


-- | (✔)
instance ParsedForest Nexus where
    unifyGraph = const Nothing -- Nexus parser isn't exporting trees yet.

