-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Binary.Fitch
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Fitch operations for binary optimization
--
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, AllowAmbiguousTypes #-}

module Analysis.Parsimony.Binary.Fitch where

import Analysis.Parsimony.Binary.Internal
import Bio.Metadata.Class (InternalMetadata(..))
import Data.Bits

-- TODO: Make all of this take weight, ignored, maybe other metadata into consideration.

-- | Preorder Fitch operation on bit-packed sequences
--   Output five-tuple is the preliminary assignment, the aligned preliminary assignment
--   the temporary storage bit, and the local cost
preorderFitchBit :: (SeqConstraint' s, InternalMetadata m s) => Double -> s -> s -> m -> (s, s, Double)
preorderFitchBit weightValue lbit rbit inChar =
    let
        alphLen = length $ alphabet inChar
        notOr = complement $ lbit .&. rbit
        union = lbit .|. rbit
        fbit = notOr .&. (snd $ fitchMasks inChar)
        rightF = blockShiftAndFold "R" "&" alphLen notOr fbit
        finalF = blockShiftAndFold "L" "|" alphLen rightF rightF
        maskF = (fst $ fitchMasks inChar) .&. finalF
        myCost = fromIntegral $ div (popCount maskF) alphLen
        weightCost = --trace ("Cost of bit ops " ++ show myCost) 
                        weightValue * myCost
        outbit = (maskF .&. union) .|. (lbit .&. rbit)
    in (outbit, finalF, weightCost)


-- | Convenience function for bit ops
blockShiftAndFold :: SeqConstraint' s => String -> String -> Int -> s -> s -> s
blockShiftAndFold sideMode foldMode alphLen inbits initVal 
    | sideMode == "L" && foldMode == "&" = f (.&.) shiftL 
    | sideMode == "R" && foldMode == "&" = f (.&.) shiftR
    | sideMode == "L" && foldMode == "|" = f (.|.) shiftL
    | sideMode == "R" && foldMode == "|" = f (.|.) shiftR
    | otherwise = error "incorrect input for block shift and fold"
    where
      f g dir = foldr (\s acc -> g acc (dir inbits s)) initVal [1 .. alphLen - 1]

-- | Postorder Fitch operation on bit-packed sequences
--   returns the final assignment sequence
postorderFitchBit :: (SeqConstraint' s, InternalMetadata m s) => s -> s -> s -> s -> s -> m -> s
postorderFitchBit myBit lBit rBit fBit pBit inChar = 
    let
        alphLen = length $ alphabet inChar
        setX = complement myBit .&. pBit
        notX = complement setX
        setG = notX .&. (snd $ fitchMasks inChar)
        rightG = blockShiftAndFold "R" "&" alphLen notX setG
        finalG = blockShiftAndFold "L" "|" alphLen rightG rightG
        fstMask = fst $ fitchMasks inChar
        maskedNotG = complement finalG .&. fstMask
        maskedNotF = complement fBit   .&. fstMask
        setS = myBit .&. (pBit .|. maskedNotG)
        sndS = setS .|. (pBit .&. fBit)
        thdS = sndS .|. (maskedNotG .&. (maskedNotF .&. (pBit .&. (lBit .|. rBit))))
    in thdS
