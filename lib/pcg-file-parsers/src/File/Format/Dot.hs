-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Dot
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module File.Format.Dot
  ( DotGraph
  , GraphID(..)
  -- ** Parse a DOT file
  , dotStreamParser
  -- ** Get useful representations from the DOT file
  , dotChildMap
  , dotParentMap
  , dotNodeSet
  , dotEdgeSet
  , toIdentifier
  ) where

import           Control.Arrow                     ((&&&))
import           Control.Monad.State
import           Data.Foldable
import           Data.GraphViz.Attributes.Complete (Attribute (Label), Label (..))
import           Data.GraphViz.Parsing
import           Data.GraphViz.Types
import           Data.GraphViz.Types.Canonical
import           Data.Key
import           Data.Map                          (Map, fromSet, insertWith)
import           Data.Monoid
import           Data.Proxy
import           Data.Set                          (Set)
import qualified Data.Set                          as S
import           Data.String
import qualified Data.Text.Lazy                    as L
import           Prelude                           hiding (lookup)
import           Text.Megaparsec                   (MonadParsec, Token, chunkToTokens, takeWhileP)


-- |
-- Parses the 'L.Text' stream from a DOT file.

-- (MonadParsec e s m, Token s ~ Char) => m
--dotParse :: Text -> Either String (DotGraph GraphID)
dotStreamParser
  :: forall e s (m :: * -> *).
     ( MonadParsec e s m
     , Token s ~ Char
     )
  => m (DotGraph GraphID)
dotStreamParser = relabelDotGraph <$> embededParser
  where
    pxy = Proxy :: Proxy s

    -- We embed the Text based parser from Data.GraphViz.Parsing inside a
    -- polymorphic MonadParsec type from Text.Megaparsec.
    --
    -- This is done so that *all* parsers share a parsing context.
    embededParser :: m (DotGraph GraphID)
    embededParser = do
        toks <- chunkToTokens pxy <$> takeWhileP Nothing (const True) :: m String
        case fst . runParser parse $ fromString toks of
          Left  err -> fail err
          Right val -> pure val


-- |
-- Takes a DotGraph and relabels the NodeID if the node has a label
relabelDotGraph :: DotGraph GraphID -> DotGraph GraphID
relabelDotGraph g =
  let
    oldGraphStatements = graphStatements g
    oldDotNodes = nodeStmts oldGraphStatements
    oldDotEdges = edgeStmts oldGraphStatements
    (newDotNodes, newDotEdges) = (`runState` oldDotEdges) $ traverse relabel oldDotNodes
    newGraphStatements = oldGraphStatements
        { nodeStmts = newDotNodes
        , edgeStmts = newDotEdges
        }
  in
    g {graphStatements = newGraphStatements}

  where
    relabel :: DotNode GraphID -> State [DotEdge GraphID] (DotNode GraphID)
    relabel dotNode
      | hasStrLabel oldID = pure dotNode
      | otherwise = case findStrLabel dotNode of
                      Nothing  -> pure dotNode
                      Just txt -> do
                        let newID = Str txt
                        modify' (fmap (renameEdge oldID newID))
                        pure $ dotNode {nodeID = newID}

     where
       oldID = nodeID dotNode

       hasStrLabel :: GraphID -> Bool
       hasStrLabel (Str _) = True
       hasStrLabel _       = False

       findStrLabel :: DotNode n -> Maybe L.Text
       findStrLabel n = getFirst (foldMap getStrLabel (nodeAttributes n))

       getStrLabel :: Attribute -> First L.Text
       getStrLabel (Label (StrLabel txt)) = First . Just $ txt
       getStrLabel _                      = mempty

       renameEdge
         :: GraphID         -- ^ The original ID to be renamed
         -> GraphID         -- ^ The new ID name to be applied
         -> DotEdge GraphID -- ^ candidate edge
         -> DotEdge GraphID -- ^ Renamed edge
       renameEdge old new edge =
         edge
           { fromNode = newFromNode
           , toNode   = newToNode
           }

         where
           newToNode   | toNode edge   == old = new
                       | otherwise            = toNode edge
           newFromNode | fromNode edge == old = new
                       | otherwise            = fromNode edge


-- |
-- Takes a 'DotGraph' parse result and returns a set of unique node identifiers.
dotNodeSet :: Ord n => DotGraph n -> Set n
dotNodeSet = foldMap (S.singleton . nodeID) . graphNodes
-- Probably want to do something convoluted like this
{-
  where
    beSmart n =
        case attributes n of
          [] -> nodeID n
          x:xs -> case fmap NE.head . sequenceA $ getLabel <$> x:|xs of
                    Nothing -> nodeID n
                    Just label -> case label of
                                    StrLabel v -> v
                                    _ -> nodeID n

    getLabel (Label x) = Just x
    getLabel _ = Nothing
-}

-- |
-- Takes a 'DotGraph' parse result and returns a set of unique edge identifiers.
dotEdgeSet :: Ord n => DotGraph n -> Set (n, n)
dotEdgeSet = foldMap (S.singleton . toEdgeIdentifier) . graphEdges
  where

    toEdgeIdentifier = fromNode &&& toNode


-- |
-- Takes a 'DotGraph' parse result and constructs a mapping from a node to it's
-- children.
dotChildMap :: Ord n => DotGraph n -> Map n (Set n)
dotChildMap = sharedWork directionality
  where
    directionality (k,v) = insertWith (<>) k (S.singleton v)


-- |
-- Takes a 'DotGraph' parse result and constructs a mapping from a node to it's
-- parents.
dotParentMap :: Ord n => DotGraph n -> Map n (Set n)
dotParentMap = sharedWork directionality
  where
    directionality (k,v) = insertWith (<>) v (S.singleton k)


-- |
-- Intelligently render a 'Data.GraphViz.Attributes.Complete.NodeLabel' of a
-- 'GraphID' to a 'String' for output.
toIdentifier :: GraphID -> String
toIdentifier (Str x) = L.unpack x
toIdentifier (Num x) = show x


-- |
-- The shared work between generating the 'dotChildMap' and 'dotParentMap'
-- functions.
sharedWork
  :: forall n. Ord n
  => ((n, n) -> Map n (Set n) -> Map n (Set n))
  -> DotGraph n
  -> Map n (Set n)
sharedWork logic dot = fromSet getAdjacency setOfNodes
  where
    -- Get the map of directed edges.
    -- Missing nodes with out degree 0.
    edgeMap      = foldr logic mempty . dotEdgeSet

    -- fold here has type :: Maybe (Set n) -> Set n
    -- returing the empty set in Nothing case.
    getAdjacency = fold . (`lookup` setOfEdges)

    setOfEdges   = edgeMap    dot

    setOfNodes   = dotNodeSet dot
