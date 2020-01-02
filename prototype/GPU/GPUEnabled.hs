{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Bio.Sequence.Coded.GPUEnabled where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as Run
import Data.Word
import Data.Bits
import Data.Maybe

type GPUCharacters b = A.Vector b

instance Eq b => Eq (GPUCharacters b) where
    (==) bit1 bit2 = A.unlift $ Run.run $ A.fold (\b1 b2 -> b1 == b2) (A.use bit1) (A.use bit2)

instance Bits b => Bits (GPUCharacters b) where
    (.&.) bit1 bit2     
        | length bit1 /= length bit2 = error ("Attempt to take and of bits of different length " ++ show (length bit1) ++ " and " ++ show (length bit2))
        | otherwise = Run.run $ A.zipWith (.&.) (A.use bit1) (A.use bit2)
    (.|.) bit1 bit2 
        | length bit1 /= length bit2 = error "Attempt to take or of bits of different length"
        | otherwise = Run.run $ A.zipWith (.|.) (A.use bit1) (A.use bit2)
    xor bit1 bit2 = (.&.) ((.|.) bit1 bit2) (complement ((.&.) bit1 bit2))
    complement inBit = Run.run $ A.map complement (A.use inBit)
    shift  = A.shift --Run.run $ A.map (`shift` s) (A.use bits)
    rotate = A.shift --Run.run $ A.map (`rotate` r) (A.use bits)
    setBit _ _= mempty -- these methods are not meaningful so they just wipe it
    bit _ = mempty
    bitSize bits 
        | A.null bits = 0
        | otherwise = A.length bits * (fromMaybe 0 . bitSizeMaybe $ bits A.! 0)
    bitSizeMaybe bits 
        | A.null bits = Just 0
        | otherwise = (A.length bits *) <$> bitSizeMaybe (bits A.! 0)
    assigned bits 
        | A.null bits = True
        | otherwise = assigned $ bits A.! 0
    popCount = A.fold (\b acc -> acc + popCount b) 0
    testBit bits index
        | A.null bits = False
        | otherwise =
          case bitSizeMaybe bits of
            Nothing      -> False
            Just numBits -> let myBit = div index numBits
                                myPos = rem index numBits
                            in testBit (bits A.! myBit) myPos