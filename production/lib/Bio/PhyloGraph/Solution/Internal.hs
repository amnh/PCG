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

import           Bio.Character.Encodable
import           Bio.Character.Parsed.Internal
import           Bio.Metadata.Internal
import           Bio.PhyloGraph.DAG    hiding (root)
import           Bio.PhyloGraph.Forest
import           Bio.PhyloGraph.Network.Class
import           Bio.PhyloGraph.Solution.Class
import qualified Bio.PhyloGraph.Solution.Metadata as MS
import           Bio.PhyloGraph.Node   hiding (children, name)
import           Control.Evaluation
import           Data.Alphabet
import           Data.Foldable
import           Data.List                    (intercalate)
import           Data.List.NonEmpty           (NonEmpty((:|)))
import           Data.HashMap.Strict          (HashMap, fromList)
import           Data.Matrix.NotStupid        (matrix)
import           Data.Monoid                  ((<>))
--import           Data.MonoTraversable
import           Data.Vector                  (Vector, (!))
import qualified Data.Vector           as V
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
   } deriving (Eq)


-- | Custom Show instance for improved sanity in debug renderings.
instance Show d => Show (Solution d) where
    show s = unlines
        [ niceList $ metadata s
        , "\nForests: "
        , niceList $ forests s
        ]
      where
        niceList :: (Foldable t, Show a) => t a -> String
        niceList = (\x -> "[" <> x <> "\n]") . intercalate "\n, " . fmap (indent . show) . toList

        indent = unlines . fmap ("  "<>) . lines


-- | (✔)
instance GeneralSolution (Solution d) (Forest d) where
    getForests     = forests
    setForests s f = s {forests = f}


-- | (✔)
instance MS.MetadataSolution (Solution d) StandardMetadata where
    getMetadata               = metadata
    setMetadata solution meta = solution {metadata = meta}


-- | (✔)
instance Arbitrary (Solution DAG) where
    arbitrary = do
      forest    <- pure <$> (arbitrary :: Gen DAG)
      meta      <- deriveDynamicMetadatas forest
      pure Solution
           { parsedChars = mempty -- We only use this for outputting, so we ignore it when testing.
           , metadata    = meta
           , forests     = pure forest
           }


-- TODO: Can this be generaized, is this correct?
-- | Derive an arbitrary metadata for dynamic characters from a 'Forest'.
deriveDynamicMetadatas :: Forest DAG -> Gen (Vector StandardMetadata)
deriveDynamicMetadatas []    = pure mempty
deriveDynamicMetadatas (x:_) = sequenceA $ V.generate (length sequenceWLOG) f
  where
    f :: Int -> Gen StandardMetadata
    f i = do
        name'       <- getNonEmpty <$> arbitrary
        stateNames' <- V.fromList  <$> vectorOf (length alphabet') (getNonEmpty <$> arbitrary)
        pure CharMeta
             { charType   = DirectOptimization
             , alphabet   = alphabet'
             , name       = name' 
             , isAligned  = False
             , isIgnored  = False
             , weight     = 1.0
             , stateNames = stateNames'
              -- TODO: try not to cry
             , fitchMasks = (encodeDynamic alphabet' badMask, encodeDynamic alphabet' badMask)
--             , fitchMasks = (encodeDynamic alphabet' ([] :: [[String]]), encodeDynamic alphabet' ([] :: [[String]]))
           --  , fitchMasks = (undefined,undefined)
             , rootCost   = 0.0
             , costs      = TCM $ matrix (length alphabet') (length alphabet') (const 1)
             }
      where
        badMask   = ("-":|[]):|[]
        character = sequenceWLOG ! i
        -- We take one less than the width here to account for the cumpulsory gap character.
        alphabet' = fromSymbols . ("-":) $ take (alphabetSize - 1) symbols
          where
            -- We don't care about the exact symbol rendering, only that they are unique and there are a correct quantity of them.
            -- We also know that the alphabet size of the generated Dynamic characters has been restricted to less than 62 in the
            -- BitMatrix library.
            alphabetSize = symbolCount character
            symbols      = fmap pure $ ['0'..'9'] <> ['A'..'Z'] <> ['a'..'z']

    sequenceWLOG = encoded leafWLOG
    leafWLOG     = getChildNodeWLOG $ root x
    getChildNodeWLOG node =
      case children node x of
        []  -> node
        c:_ -> getChildNodeWLOG c


-- TODO: Is this correct? Do we need to derived generic Parsed values?
-- | Derive 'Arbitrary' parsed character from the supplied metadata structures.
arbitraryCharsGivenMeta :: Vector StandardMetadata -> Gen Parsed
arbitraryCharsGivenMeta allMeta = do
    names         <- listOf (arbitrary :: Gen String)
    let nodeCount = length names
    let alphs     = toList $ V.map alphabet allMeta
    parsed        <- vectorOf nodeCount (parsedCharsGivenAlph alphs)
    pure . fromList $ zip names parsed
