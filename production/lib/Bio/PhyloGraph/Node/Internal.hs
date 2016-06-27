-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.PhyloGraph.Node.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Type for a node
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Bio.PhyloGraph.Node.Internal where

import           Bio.Character.Dynamic.Coded
import qualified Bio.PhyloGraph.Node.Encoded      as EN
import qualified Bio.PhyloGraph.Node.Final        as FN
import qualified Bio.PhyloGraph.Node.ImpliedAlign as IN
import qualified Bio.PhyloGraph.Node.Packed       as PN
import qualified Bio.PhyloGraph.Node.Preliminary  as RN
import           Bio.PhyloGraph.Node.Referential
import           Data.Alphabet
import           Data.Foldable
import           Data.Vector                            (Vector)
import qualified Data.Vector                      as V  (fromList)
import           Data.Ord ()
import           Test.Tasty.QuickCheck

-- | A node data structure holding all the necessary info (add verbose statement about what each field is)
data Node = Node  { nodeIdx             :: Int
                  , name                :: String
                  , isRoot              :: Bool
                  , isLeaf              :: Bool
                  , parents             :: [Int]
                  , children            :: [Int]
                  , encoded             :: Vector DynamicChar -- leaf only, contains assignment from input
                  , packed              :: Vector DynamicChar -- packed version of the sequence; for Fitch only?
                  , random              :: Vector DynamicChar -- the assignment with a single state randomly selected to remove ambiguity
                  , union               :: Vector DynamicChar -- the union assignment
                  , single              :: Vector DynamicChar -- the single assignment
                  --, temporary           :: Vector DynamicChar -- multipurpose temporary assignment
                  , preliminaryUngapped :: Vector DynamicChar -- ungapped assignment, after preorder traversal
                  , finalUngapped       :: Vector DynamicChar -- ungapped final assignment, after postorder traversal
                  , preliminaryGapped   :: Vector DynamicChar -- gapped alignment from preorder travesal
                  , finalGapped         :: Vector DynamicChar -- gapped final assignment, for use in IA
                  , iaHomology          :: IN.HomologyTrace   -- the homology traces for an implied alignment (the matrix is numChars by charLength). might be deprecated
                  , impliedAlignment    :: Vector DynamicChar -- the homology traces for an implied alignment (the matrix is numChars by charLength)
                  , localCost           :: Double             -- cost of assignment at this node alone
                  , totalCost           :: Double             -- sum cost of this node and its subtree
                  } deriving (Eq, Show)

-- | Make it an instance of encoded, final, packed, and preliminary
instance EN.EncodedNode Node DynamicChar where
    getEncoded     = encoded
    setEncoded n s = n {encoded = s}

-- | Nodes can hold final assignment
instance FN.FinalNode Node DynamicChar where
    getFinal           = finalUngapped
    setFinal f n       = n {finalUngapped = f}
    getFinalGapped     = finalGapped
    setFinalGapped f n = n {finalGapped = f}

-- | Nodes can hold packed data
instance PN.PackedNode Node DynamicChar where
    getPacked     = packed
    setPacked n s = n {packed = s}

-- | Nodes hold all preliminary info
instance RN.PreliminaryNode Node DynamicChar where
    getPreliminaryUngapped     = preliminaryUngapped
    setPreliminaryUngapped s n = n {preliminaryUngapped = s}
    getPreliminaryGapped       = preliminaryGapped
    setPreliminaryGapped s n   = n {preliminaryGapped = s}
    -- getTemporary               = temporary
    -- setTemporary s n           = n {temporary = s}
    getLocalCost               = localCost
    setLocalCost c n           = n {localCost = c}
    getTotalCost               = totalCost
    setTotalCost c n           = n {totalCost = c}

instance IN.IANode Node where
  getHomologies = iaHomology
  setHomologies n h = n {iaHomology = h}

instance IN.IANode' Node DynamicChar where
  getHomologies'             = impliedAlignment
  setHomologies' n alignment = n { impliedAlignment = alignment }

instance RefNode Node where
  getCode = nodeIdx

{-
instance Arbitrary Node where
    arbitrary = do
        c                <- arbitrary :: Gen Int
        n                <- arbitrary :: Gen String
        root             <- arbitrary :: Gen Bool
        leaf             <- arbitrary :: Gen Bool
        child            <- listOf (arbitrary :: Gen Int)
        parent           <- listOf $ suchThat arbitrary (not . flip elem child)
        groupOfSequences <- vectorOf 10 arbitrary
        c2               <- arbitrary :: Gen Double
        c3               <- arbitrary :: Gen Double
        pure Node 
                 { nodeIdx     = c
                 , name        = n
                 , isRoot      = root
                 , isLeaf      = leaf
                 , children    = child
                 , parents     = parent
                 , encoded     = head groupOfSequences
                 , packed      = groupOfSequences !! 1
                 , preliminaryUngapped = groupOfSequences !! 2
                 , finalUngapped       = groupOfSequences !! 3
                 , temporary   = groupOfSequences !! 4
                 , preliminaryGapped     = groupOfSequences !! 5
                 , random      = groupOfSequences !! 6
                 , union       = groupOfSequences !! 7
                 , single      = groupOfSequences !! 8
                 , finalGapped      = groupOfSequences !! 9
                 , iaHomology  = mempty
                 , impliedAlignment = mempty
                 , localCost   = c2
                 , totalCost   = c3
                 }
-}

generateLeavesDO :: Alphabet String -> Int -> Gen [Node]
generateLeavesDO alphabet taxaCount = do
      sequenceLength  <- choose (1,2)
      sequence $ generateLeaf sequenceLength <$> [0..taxaCount-1]
    where
        generateDynamicCharacter :: Gen DynamicChar
        generateDynamicCharacter = do
            dynamicCharacterLength <- choose (1,2) :: Gen Int
            fmap (encodeDynamic alphabet) . vectorOf dynamicCharacterLength . sublistOf $ toList alphabet
        generateLeaf sequenceLength i = do
            sequenceOfEncodedDynamicChars <- V.fromList <$> vectorOf sequenceLength generateDynamicCharacter 
            pure Node 
                 { nodeIdx             = i
                 , name                = show i
                 , isRoot              = False
                 , isLeaf              = True
                 , children            = []
                 , parents             = []
                 , encoded             = sequenceOfEncodedDynamicChars
                 , packed              = mempty
                 , preliminaryUngapped = mempty
                 , finalUngapped       = mempty
                 --, temporary           = mempty
                 , preliminaryGapped   = mempty
                 , random              = mempty
                 , union               = mempty
                 , single              = mempty
                 , finalGapped         = mempty
                 , iaHomology          = mempty
                 , impliedAlignment    = mempty
                 , localCost           = 0
                 , totalCost           = 0
                 }
