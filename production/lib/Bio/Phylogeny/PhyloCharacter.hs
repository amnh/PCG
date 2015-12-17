-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Megaparsec.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Data structure for a PhyloCharacter character info
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

module Bio.Phylogeny.PhyloCharacter where

import Data.Vector
import GHC.Generics
import Data.Matrix (Matrix)

-- | Define a character type as DNA, RNA, Morphology, Continous, or Custom
-- Let it hold whether its aligned, masks for evaluation, its alphabet, and a cost matrix
data PhyloCharacter b = DNA {aligned :: Bool, masks :: (Vector b, Vector b), alphabet :: Vector String, tcm :: CostMatrix}
                      | RNA {aligned :: Bool, masks :: (Vector b, Vector b), alphabet :: Vector String, tcm :: CostMatrix}
                      | Morphology {aligned :: Bool, masks :: (Vector b, Vector b), alphabet :: Vector String}
                      | Continous 
                      | Custom {aligned :: Bool, masks :: (Vector b, Vector b), alphabet :: Vector String, tcm :: CostMatrix} 
                            deriving (Show, Eq, Generic)

type CostMatrix = Matrix Float


--class PhyloCharacter a b | a -> b where
--    aligned     :: a -> Bool
--    setAligned  :: a -> Bool -> a
--    masks       :: a -> Maybe (Vector b, Vector b)
--    setMasks    :: a -> Maybe (Vector b, Vector b) -> a
--    alphabet    :: a -> Vector String
--    setAlphabet :: a -> Vector String -> a
--    charType    :: a -> RF.CharType
--    setCharType :: a -> RF.CharType -> a

--instance PhyloCharacter RF.CharInfo Int64 where
--    aligned n = True
--    setAligned n _ = n
--    alphabet n = fromList $ RF.alphabet n
--    setAlphabet n alph = n {RF.alphabet = toList alph}
--    charType = RF.charType
--    setCharType n val = n {RF.charType = val}
--    masks = const Nothing
--    setMasks n _ = n
