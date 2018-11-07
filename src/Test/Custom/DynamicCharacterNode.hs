-----------------------------------------------------------------------------
-- |
-- Module      :  Test.Custom.DynamicCharacterNode
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Arbitrary instance for a cherry of dynamic characters.
--
-- Allows for base ambiguities and gaps. The dynamic characters will be
-- non-empty and non-missing.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Test.Custom.DynamicCharacterNode
  ( DynamicCharacterNode()
  , getDynamicCharacterDecoration
  , constructNode
  ) where


import Analysis.Parsimony.Dynamic.DirectOptimization
import Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise (filterGaps)
import Analysis.Parsimony.Internal
import Bio.Character
import Bio.Character.Decoration.Dynamic
import Bio.Metadata
import Bio.Metadata.CharacterName
import Data.Alphabet
import Data.MonoTraversable
import Data.String
import Test.Custom.NucleotideSequence
import Test.QuickCheck


-- |
-- Represents a cherry node of dynamic character decorations.
newtype DynamicCharacterNode = DCN
    { getDynamicCharacterDecoration :: DynamicDecorationDirectOptimization DynamicCharacter -- ^ Extract the character decoration.
    }
    deriving (Show)


instance Arbitrary DynamicCharacterNode where

    arbitrary = do
        lhs <- (unwrap <$> arbitrary) `suchThat` isNotAllGaps
        rhs <- (unwrap <$> arbitrary) `suchThat` isNotAllGaps
        pure . DCN $ constructNode lhs rhs
      where
        isNotAllGaps  = not . onull . filterGaps
        unwrap (NS x) = x


-- |
-- Given two dynamic characters, constructs a cherry node with each character as
-- a child.
constructNode :: DynamicCharacter -> DynamicCharacter -> DynamicDecorationDirectOptimization DynamicCharacter
constructNode lhs rhs
    = directOptimizationPreorder pairwiseFunction defMetadata
        (PreInternalContext
           { preParent       = rootDec
           , preChildContext = Left lhsDec
           }
        )
  where
    lhsDec  = toLeafNode $ initDec lhs
    rhsDec  = toLeafNode $ initDec rhs
    rootDec = toRootNode lhsDec rhsDec


toLeafNode
  :: ( Ord (Element c)
     , SimpleDynamicDecoration d c
--     , GetSparseTransitionCostMatrix (DynamicCharacterMetadataDec (Element c)) MemoizedCostMatrix
     )
  => d
  -> DynamicDecorationDirectOptimizationPostorderResult c
toLeafNode c = directOptimizationPostorder pairwiseFunction (LeafContext c)


toRootNode :: DynamicDecorationDirectOptimizationPostorderResult DynamicCharacter
           -> DynamicDecorationDirectOptimizationPostorderResult DynamicCharacter
           -> DynamicDecorationDirectOptimization DynamicCharacter
toRootNode x y = directOptimizationPreorder pairwiseFunction defMetadata (RootContext z)
  where
    z :: DynamicDecorationDirectOptimizationPostorderResult DynamicCharacter
    z = directOptimizationPostorder
          pairwiseFunction
          (PostBinaryContext {binNode = e, leftChild = x , rightChild = y})
    e :: DynamicDecorationDirectOptimizationPostorderResult DynamicCharacter
    e = undefined


pairwiseFunction :: ( Ord (Element s)
                    , EncodableDynamicCharacter s
                    ) => s -> s -> (Word, s, s, s, s)
pairwiseFunction x y = naiveDO x y scm


scm :: Word -> Word -> Word
scm i j = if i == j then 0 else 1


defMetadata :: DynamicCharacterMetadataDec (Element DynamicCharacter)
defMetadata = dynamicMetadata defName defWeight defAlphabet scm Nothing


initDec :: DynamicCharacter -> DynamicDecorationInitial DynamicCharacter
initDec = toDynamicCharacterDecoration id


defName :: CharacterName
defName = fromString "Test Character"


defWeight :: Double
defWeight = 1


defAlphabet :: Alphabet String
defAlphabet = fromSymbols ["A","C","G","T"]

