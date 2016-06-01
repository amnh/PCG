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
import Bio.Metadata
import Data.Bits
import Data.MonoTraversable

import Debug.Trace

-- | Preorder Fitch operation on bit-packed sequences
--   Output three-tuple is the preliminary assignment, the temporary assignment, and the cost
preorderFitchBit :: (SeqConstraint' s, Metadata m s) => Double -> s -> s -> m -> (s, s, Double)
preorderFitchBit _ _ _ c | trace ("masks for fitch " ++ show (getFitchMasks c)) False = undefined
preorderFitchBit weightValue lbit rbit inChar =
    let
        alphLen = length $ getAlphabet inChar
        notOr = complement $ lbit .&. rbit
        union = {-trace ("notOr " ++ show notOr ++ " on and " ++ show (lbit .&. rbit)) $ -} lbit .|. rbit
        fbit = {-trace ("union " ++ show union) $ -} notOr .&. snd (getFitchMasks inChar)
        rightF = {-trace ("fbit " ++ show fbit) $ -}blockShiftAndFold "R" "&" alphLen notOr fbit
        finalF = {-trace ("rightF " ++ show rightF) $ -}blockShiftAndFold "L" "|" alphLen rightF rightF
        maskF = --trace ("maksed f value " ++ show finalF) $
                    fst (getFitchMasks inChar) .&. finalF
        myCost = {-trace ("maskF " ++ show maskF) $ -}fromIntegral $ div (popCount maskF) alphLen
        weightCost = --trace ("Cost of bit ops " ++ show myCost) 
                        weightValue * myCost
        outbit = (maskF .&. union) .|. (lbit .&. rbit)
    in --trace ("outBit of Fitch " ++ show outbit)
        (outbit, maskF, weightCost)


-- | Convenience function for bit ops
-- Performs a series of shifts and folds together into a single chunk
-- Takes in the mode expressed as two strings, the alphabet length, the input sequence, and the thing to fold into
-- outputs a final sequence
blockShiftAndFold :: SeqConstraint' s => String -> String -> Int -> s -> s -> s
--blockShiftAndFold _ _ _ b i | trace ("block shift and fold " ++ show b ++ " on " ++ show i) False = undefined
blockShiftAndFold sideMode foldMode alphLen inbits initVal 
    | sideMode == "L" && foldMode == "&" = f (.&.) (\s n -> omap (`shiftL` n) s)
    | sideMode == "R" && foldMode == "&" = f (.&.) (\s n -> omap (`shiftR` n) s)
    | sideMode == "L" && foldMode == "|" = f (.|.) (\s n -> omap (`shiftL` n) s)
    | sideMode == "R" && foldMode == "|" = f (.|.) (\s n -> omap (`shiftR` n) s)
    | otherwise = error "incorrect input for block shift and fold"
    where
      f g dir = foldr (\s acc -> g acc (dir inbits s)) initVal [1 .. alphLen - 1]

-- | Postorder Fitch operation on bit-packed sequences
-- Takes in the preliminary assignment, assignments of left and right children, the temporary assignment, 
-- the parent assignment, and the metadata
--   returns the final assignment sequence
postorderFitchBit :: (SeqConstraint' s, Metadata m s) => s -> s -> s -> s -> s -> m -> s
postorderFitchBit myBit lBit rBit fBit pBit inChar = 
    let
        alphLen = length $ getAlphabet inChar
        setX = complement myBit .&. pBit
        notX = complement setX
        setG = notX .&. snd (getFitchMasks inChar)
        rightG = blockShiftAndFold "R" "&" alphLen notX setG
        finalG = blockShiftAndFold "L" "|" alphLen rightG rightG
        fstMask = fst $ getFitchMasks inChar
        maskedNotG = complement finalG .&. fstMask
        maskedNotF = complement fBit   .&. fstMask
        setS = myBit .&. (pBit .|. maskedNotG)
        sndS = setS .|. (pBit .&. fBit)
        thdS = sndS .|. (maskedNotG .&. (maskedNotF .&. (pBit .&. (lBit .|. rBit))))
    in thdS
