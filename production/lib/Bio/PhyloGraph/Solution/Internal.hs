-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Solution.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for Solution representation
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.PhyloGraph.Solution.Internal where

import           Bio.PhyloGraph.DAG           hiding (root)
import           Bio.PhyloGraph.Forest
import           Bio.PhyloGraph.Network.Class
import           Bio.PhyloGraph.Solution.Class
import qualified Bio.PhyloGraph.Solution.Metadata as MS
import           Bio.Character.Parsed.Internal
import           Bio.Character.Dynamic.Coded
import           Bio.Metadata.Internal
import           Bio.PhyloGraph.Node hiding (children, name)

import           Control.Evaluation
import           Data.Alphabet
import           Data.BitVector (width)
import           Data.Foldable
import           Data.HashMap.Strict (HashMap, fromList)
import           Data.Matrix.NotStupid (matrix)
import           Data.Monoid ((<>))
import           Data.MonoTraversable
import           Data.Vector      (Vector, (!))
import qualified Data.Vector as V
import           Test.Tasty.QuickCheck 

-- | The equatable identifier for a node in the graph.
type Identifier = String

-- TODO: ParsedDynChars should probably not be hard coded here.
-- TODO: Actually, why do we need this at all?
-- | The sequence of characters associated with a taxon.
type Sequences = ParsedChars

-- We'll have two types of node: topological and referential

-- TODO: DynamicChar should probably not be hard coded here.
-- | The character metadata reference structure.
type StandardMetadata = CharacterMetadata DynamicChar

-- | A simple storable computation state value.
type StandardSolution = Solution DAG

-- | A computational evaluation state which can be modified monoidally or
--   monadically.
type SearchState = EvaluationT IO StandardSolution

-- | A structure for storing parsed characters
type Parsed = HashMap Identifier Sequences

-- | A solution is an array of forests character data and names are common
--   across all forests and so stored at this level
data Solution d
   = Solution
   { parsedChars :: Parsed
   , metadata    :: Vector StandardMetadata
   , forests     :: [Forest d]
   } deriving (Eq, Show)

-- | Make it an instance of data storage type classes

instance GeneralSolution (Solution d) (Forest d) where
    getForests     = forests
    setForests s f = s {forests = f}

instance MS.MetadataSolution (Solution d) StandardMetadata where
    getMetadata               = metadata
    setMetadata solution meta = solution {metadata = meta}

instance Arbitrary (Solution DAG) where
    arbitrary = do
      forest    <- pure <$> (arbitrary :: Gen DAG)
      meta      <- deriveDynamicMetadatas forest
      pure $ Solution
           { parsedChars = mempty -- We only use this for outputting, so we ignore it when testing.
           , metadata    = meta
           , forests     = pure forest
           }

deriveDynamicMetadatas :: Forest DAG -> Gen (Vector StandardMetadata)
deriveDynamicMetadatas []     = pure mempty
deriveDynamicMetadatas (x:xs) = sequenceA $ V.generate (length sequenceWLOG) f
  where
    f :: Int -> Gen StandardMetadata
    f i = do
        name'       <- getNonEmpty <$> arbitrary
        stateNames' <- V.fromList <$> vectorOf (length alphabet') (getNonEmpty <$> arbitrary)
        pure $ CharMeta
             { charType   = DirectOptimization
             , alphabet   = alphabet'
             , name       = name' 
             , isAligned  = False
             , isIgnored  = False
             , weight     = 1.0
             , stateNames = stateNames'
             , fitchMasks = (encodeDynamic alphabet' ([] :: [[String]]), encodeDynamic alphabet' ([] :: [[String]]))
             , rootCost   = 0.0
             , costs      = TCM $ matrix (length alphabet') (length alphabet') (const 1.0)
             }
      where
        character = sequenceWLOG ! i
        -- We take one less than the width here to account for the cumpulsory gap character.
        alphabet' = constructAlphabet . ("-":) $ take (alphabetSize - 1) symbols
          where
            -- We don't care about the exact symbol rendering, only that they are unique and there are a correct quantity of them.
            -- We also know that the alphabet size of the generated Dynamic characters has been restricted to less than 62 in the
            -- BitMatrix library.
            alphabetSize = width $ character `indexChar` 0
            symbols      = fmap pure $ ['0'..'9'] <> ['A'..'Z'] <> ['a'..'z']

    sequenceWLOG = encoded leafWLOG
    leafWLOG     = getChildNodeWLOG $ root x
    getChildNodeWLOG node =
      case children node x of
        []  -> node
        c:_ -> getChildNodeWLOG c

arbitraryCharsGivenMeta :: Vector StandardMetadata -> Gen Parsed
arbitraryCharsGivenMeta allMeta = do
  names <- listOf (arbitrary :: Gen String)
  let numNodes = length names
  let alphs = toList $ V.map alphabet allMeta
  parsed <- vectorOf numNodes (parsedCharsGivenAlph alphs)
  pure $ fromList $ zip names parsed
