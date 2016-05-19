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

import Data.Alphabet
import Data.Foldable
import Data.Vector (Vector, (!))
import qualified Data.Vector as V (fromList)
import Data.Monoid
import Data.Ord ()
import Test.Tasty.QuickCheck

-- | A node data structure holding all the necessary info (add verbose statement about what each field is)
data Node = Node  { code        :: Int
                  , name        :: String
                  , isRoot      :: Bool
                  , isLeaf      :: Bool
                  , parents     :: [Int]
                  , children    :: [Int]
                  , encoded     :: Vector DynamicChar -- encoded version of original assignment
                  , packed      :: Vector DynamicChar -- packed version of the sequence
                  , preliminary :: Vector DynamicChar -- preliminary assignment at a node
                  , final       :: Vector DynamicChar -- final assignment at a node
                  , temporary   :: Vector DynamicChar -- multipurpose temporary assignment
                  , aligned     :: Vector DynamicChar -- the alignment between the children
                  , random      :: Vector DynamicChar -- the assignment with a single state randomly selected to remove ambiguity
                  , union       :: Vector DynamicChar -- the union assignment
                  , single      :: Vector DynamicChar -- the single assignment
                  , gapped      :: Vector DynamicChar -- the final assignment with gaps for alignment
                  , iaHomology  :: IN.HomologyTrace  -- the homology traces for an implied alignment (the matrix is numChars by charLength)
                  , localCost   :: Double            -- cost of assignment at this node alone
                  , totalCost   :: Double            -- sum cost of this node and its subtree
                  } deriving (Eq, Show)

-- | Make it an instance of encoded, final, packed, and preliminary
instance EN.EncodedNode Node DynamicChar where
    getEncoded     = encoded
    setEncoded n s = n {encoded = s}

-- | Nodes can hold final assignment
instance FN.FinalNode Node DynamicChar where
    getFinal           = final
    setFinal f n       = n {final = f}
    getFinalGapped     = gapped
    setFinalGapped f n = n {gapped = f}

-- | Nodes can hold packed data
instance PN.PackedNode Node DynamicChar where
    getPacked     = packed
    setPacked n s = n {packed = s}

-- | Nodes hold all preliminary info
instance RN.PreliminaryNode Node DynamicChar where
    getPreliminary      = preliminary
    setPreliminary s n  = n {preliminary = s}
    getPreliminaryAlign = aligned
    setAlign s n        = n {aligned = s}
    getTemporary        = temporary
    setTemporary s n    = n {temporary = s}
    getLocalCost        = localCost
    setLocalCost c n    = n {localCost = c}
    getTotalCost        = totalCost
    setTotalCost c n    = n {totalCost = c}

instance IN.IANode Node where
  getHomologies = iaHomology
  setHomologies n h = n {iaHomology = h}

instance RefNode Node where
  getCode = code

instance Arbitrary Node where
    arbitrary = do
        c      <- arbitrary :: Gen Int
        n      <- arbitrary :: Gen String
        root   <- arbitrary :: Gen Bool
        leaf   <- arbitrary :: Gen Bool
        child  <- listOf (arbitrary :: Gen Int)
        parent <- listOf $ suchThat arbitrary (not . flip elem child)
        groupOfSequences   <- vectorOf 10 arbitrary
        c2     <- arbitrary :: Gen Double
        c3     <- arbitrary :: Gen Double
        pure $ Node c n root leaf child parent (head groupOfSequences ) (groupOfSequences !! 1) (groupOfSequences !! 2) (groupOfSequences !! 3) (groupOfSequences !! 4) (groupOfSequences !! 5) (groupOfSequences !! 6) (groupOfSequences !! 7) (groupOfSequences !! 8) (groupOfSequences !! 9) mempty c2 c3

generateLeavesDO :: Alphabet String -> Int -> Gen [Node]
generateLeavesDO alphabet taxaCount = do
      sequence $ generateLeaf <$> [0..taxaCount-1]
    where
        generateDynamicCharacter :: Gen DynamicChar
        generateDynamicCharacter = do
            dynamicCharacterLength <- choose (1,100) :: Gen Int
            fmap (encodeDynamic alphabet) . vectorOf dynamicCharacterLength . sublistOf $ toList alphabet
        generateLeaf i = do
            sequenceLength  <- choose (1,25)
            sequenceOfEncodedDynamicChars <- V.fromList <$> vectorOf sequenceLength generateDynamicCharacter 
            pure $ Node 
                 { code        = i
                 , name        = show i
                 , isRoot      = False
                 , isLeaf      = True
                 , children    = []
                 , parents     = []
                 , encoded     = sequenceOfEncodedDynamicChars
                 , packed      = mempty
                 , preliminary = mempty
                 , final       = mempty
                 , temporary   = mempty
                 , aligned     = mempty
                 , random      = mempty
                 , union       = mempty
                 , single      = mempty
                 , gapped      = mempty
                 , iaHomology  = mempty
                 , localCost   = 0
                 , totalCost   = 0
                 }