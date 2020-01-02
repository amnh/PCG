{-# LANGUAGE BangPatterns, DeriveGeneric #-}
module PackingPar.GPUNode ((.&.), 
                (.|.), 
                complement, 
                popCount, 
                blockShiftAndFold, 
                genMasks, 
                displayNode,
                getNodeCost,
                shift,
                bitSize) where

-- | imports
import Packing.CardinalityLookup
import qualified Data.Bits as B
import Data.Word
import qualified Data.Vector as V
import qualified Data.BitVector as BV
import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.Either
import Debug.Trace
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as AI
import Control.Parallel.Strategies
import           GHC.Generics
import Control.DeepSeq
import Packing.BitPackedNode (BitPackedNode (EmptyPackNode, A16, A64, S16, S64, SInf), PackMode (bitLen, adaptive))

-- | make the cardinality table and the masks for 64 bit cardinality
-- All cardinalities except for the "infinite" type are from the stored table
fth16, thd16, snd16, fst16 :: Word64
allSelect :: V.Vector Word64
cardTable :: V.Vector Int
cardTable = makeLookup 
fth16 = (foldr (\i acc -> acc + 2^i) (0 :: Word64) ([48 .. 64] :: [Int])) 
thd16 = (foldr (\i acc -> acc + 2^i) (0 :: Word64) ([32 .. 47] :: [Int])) 
snd16 = (foldr (\i acc -> acc + 2^i) (0 :: Word64) ([16 .. 31] :: [Int])) 
fst16 = (foldr (\i acc -> acc + 2^i) (0 :: Word64) ([0 .. 15] :: [Int]))
allSelect = V.fromList [fst16, snd16, thd16, fth16] 


-- | And function: throws errors for different length bits, and of any empty node returns an empty
(.&.) EmptyPackNode _ = EmptyPackNode
(.&.) _ EmptyPackNode = EmptyPackNode
(.&.) (A16 bit1) (A16 bit2) = 
    let 
        shape = A.Z A.:. (V.length bit1)
        array1 = A.fromList shape (V.toList bit1) :: A.Vector A.Word16 
        array2 = A.fromList shape (V.toList bit2) :: A.Vector A.Word16
        a     | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
            | otherwise = AI.run $ A.zipWith (\b1 b2 ->b1 B..&. b2) (A.use array1) (A.use array2)
    in --trace ("and node " ++ show a ++ " from bit " ++ show array1 ++ "  and bit " ++ show array2)
        A16 $ V.fromList $ A.toList a
(.&.) (S16 bit1) (S16 bit2) = 
    let 
        shape = A.Z A.:. (V.length bit1)
        array1 = A.fromList shape (V.toList bit1) :: A.Vector A.Word16 
        array2 = A.fromList shape (V.toList bit2) :: A.Vector A.Word16
        a     | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
            | otherwise = AI.run $ A.zipWith (\b1 b2 -> b1 B..&. b2) (A.use array1) (A.use array2)
    in S16 $ V.fromList $ A.toList a
(.&.) (A64 bit1) (A64 bit2) = 
    let 
        shape = A.Z A.:. (V.length bit1)
        array1 = A.fromList shape (V.toList bit1) :: A.Vector A.Word64
        array2 = A.fromList shape (V.toList bit2) :: A.Vector A.Word64
        a     | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
            | otherwise = AI.run $ A.zipWith (\b1 b2 -> b1 B..&. b2) (A.use array1) (A.use array2)
    in A64 $ V.fromList $ A.toList a
(.&.) (S64 bit1) (S64 bit2) = 
    let 
        shape = A.Z A.:. (V.length bit1)
        array1 = A.fromList shape (V.toList bit1) :: A.Vector A.Word64
        array2 = A.fromList shape (V.toList bit2) :: A.Vector A.Word64
        a     | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
            | otherwise = AI.run $ A.zipWith (\b1 b2 -> b1 B..&. b2) (A.use array1) (A.use array2)
    in S64 $ V.fromList $ A.toList a
(.&.) (SInf bit1) (SInf bit2) = 
    let
        a     | (BV.size bit1) /= (BV.size bit2) = error "Attempt to take and of bits of different lengths"
            | otherwise = bit1 B..&. bit2
    in SInf a
(.&.) _ _ = error "Attempt to take and of two different node types"

        

-- | Or function: throws errors for different length bits, and of any empty node returns an empty
(.|.) EmptyPackNode _ = EmptyPackNode
(.|.) _ EmptyPackNode = EmptyPackNode
(.|.) (A16 bit1) (A16 bit2) = 
    let 
        shape = A.Z A.:. (V.length bit1)
        array1 = A.fromList shape (V.toList bit1) :: A.Vector A.Word16 
        array2 = A.fromList shape (V.toList bit2) :: A.Vector A.Word16
        a     | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
            | otherwise = AI.run $ A.zipWith (\b1 b2 -> b1 B..|. b2) (A.use array1) (A.use array2)
    in A16 $ V.fromList $ A.toList a
(.|.) (S16 bit1) (S16 bit2) = 
    let 
        shape = A.Z A.:. (V.length bit1)
        array1 = A.fromList shape (V.toList bit1) :: A.Vector A.Word16 
        array2 = A.fromList shape (V.toList bit2) :: A.Vector A.Word16
        a     | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
            | otherwise = AI.run $ A.zipWith (\b1 b2 -> b1 B..|. b2) (A.use array1) (A.use array2)
    in S16 $ V.fromList $ A.toList a
(.|.) (A64 bit1) (A64 bit2) = 
    let 
        shape = A.Z A.:. (V.length bit1)
        array1 = A.fromList shape (V.toList bit1) :: A.Vector A.Word64
        array2 = A.fromList shape (V.toList bit2) :: A.Vector A.Word64
        a     | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
            | otherwise = AI.run $ A.zipWith (\b1 b2 -> b1 B..|. b2) (A.use array1) (A.use array2)
    in A64 $ V.fromList $ A.toList a
(.|.) (S64 bit1) (S64 bit2) = 
    let 
        shape = A.Z A.:. (V.length bit1)
        array1 = A.fromList shape (V.toList bit1) :: A.Vector A.Word64
        array2 = A.fromList shape (V.toList bit2) :: A.Vector A.Word64
        a     | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
            | otherwise = AI.run $ A.zipWith (\b1 b2 -> b1 B..|. b2) (A.use array1) (A.use array2)
    in S64 $ V.fromList $ A.toList a
(.|.) (SInf bit1) (SInf bit2) = 
    let
        a     | (BV.size bit1) /= (BV.size bit2) = error "Attempt to take and of bits of different lengths"
            | otherwise = bit1 B..|. bit2
    in SInf a
(.|.) _ _ = error "Attempt to take or of two different node types"

-- | xor function as a combination of and, or
xor EmptyPackNode _ = EmptyPackNode
xor _ EmptyPackNode = EmptyPackNode
xor bit1 bit2 = (bit1 .|. bit2) .&. (complement (bit1 .&. bit2))

-- | complement function turning on opposite bits
complement EmptyPackNode = EmptyPackNode
complement (A16 b) = 
    let
        shape = A.Z A.:. (V.length b)
        array = A.fromList shape (V.toList b) :: A.Vector A.Word16
    in A16 $ V.fromList $ A.toList $ AI.run $ A.map (\bs -> B.complement bs) (A.use array)
complement (S16 b) = 
    let
        shape = A.Z A.:. (V.length b)
        array = A.fromList shape (V.toList b) :: A.Vector A.Word16
    in S16 $ V.fromList $ A.toList $ AI.run $ A.map (\bs -> B.complement bs) (A.use array)
complement (A64 b) = 
    let
        shape = A.Z A.:. (V.length b)
        array = A.fromList shape (V.toList b) :: A.Vector A.Word64
    in A64 $ V.fromList $ A.toList $ AI.run $ A.map (\bs -> B.complement bs) (A.use array)
complement (S64 b) = 
    let
        shape = A.Z A.:. (V.length b)
        array = A.fromList shape (V.toList b) :: A.Vector A.Word64
    in S64 $ V.fromList $ A.toList $ AI.run $ A.map (\bs -> B.complement bs) (A.use array)
complement (SInf b) = SInf $ B.complement b

-- | Generic shift function, a negative value causes a right shift and a positive a right shift
shift EmptyPackNode _ = EmptyPackNode
shift (A16 b) s = 
    let
        shape = A.Z A.:. (V.length b)
        array = A.fromList shape (V.toList b) :: A.Vector A.Word16
    in A16 $ V.fromList $ A.toList $ AI.run $ A.map (\b -> B.shift b s) (A.use array)
--shift (A16 b) s = A16 $ V.map (\bs -> shift bs s) b 
shift (S16 b) s = --S16 $ V.map (\bs -> shift bs s) b 
    let
        shape = A.Z A.:. (V.length b)
        array = A.fromList shape (V.toList b) :: A.Vector A.Word16
    in S16 $ V.fromList $ A.toList $ AI.run $ A.map (\b -> B.shift b s) (A.use array)
shift (A64 b) s = --A64 $ V.map (\bs -> shift bs s) b 
    let
        shape = A.Z A.:. (V.length b)
        array = A.fromList shape (V.toList b) :: A.Vector A.Word64
    in A64 $ V.fromList $ A.toList $ AI.run $ A.map (\b -> B.shift b s) (A.use array)
shift (S64 b) s = --S64 $ V.map (\bs -> shift bs s) b 
    let
        shape = A.Z A.:. (V.length b)
        array = A.fromList shape (V.toList b) :: A.Vector A.Word64
    in S64 $ V.fromList $ A.toList $ AI.run $ A.map (\b -> B.shift b s) (A.use array)
shift (SInf b) s = SInf $ B.shift b s

-- | Rotate function for bits
rotate EmptyPackNode _ = EmptyPackNode
rotate (A16 b) r =
    let
        shape = A.Z A.:. (V.length b)
        array = A.fromList shape (V.toList b) :: A.Vector A.Word16
    in A16 $ V.fromList $ A.toList $ AI.run $ A.map (\bs -> B.rotate bs r) (A.use array)
rotate (S16 b) r =
    let
        shape = A.Z A.:. (V.length b)
        array = A.fromList shape (V.toList b) :: A.Vector A.Word16
    in S16 $ V.fromList $ A.toList $ AI.run $ A.map (\bs -> B.rotate bs r) (A.use array)
rotate (A64 b) r = 
    let
        shape = A.Z A.:. (V.length b)
        array = A.fromList shape (V.toList b) :: A.Vector A.Word64
    in A64 $ V.fromList $ A.toList $ AI.run $ A.map (\bs -> B.rotate bs r) (A.use array)
rotate (S64 b) r = 
    let
        shape = A.Z A.:. (V.length b)
        array = A.fromList shape (V.toList b) :: A.Vector A.Word64
    in S64 $ V.fromList $ A.toList $ AI.run $ A.map (\bs -> B.rotate bs r) (A.use array)
rotate (SInf b) r = SInf $ B.rotate b r

-- | Both setBit and bit i are not meaningful for these data structures (could be a variety of things) 
-- and so simply return an empty
-- NOTE THAT setBIT IS NOT MEANINGFUL FOR THIS DATA STRUCTURE
setBit _ _ = EmptyPackNode

bit _ = EmptyPackNode -- this is not meaningful for the bit vectors so just always give back an empty

-- | The b size is the number of slots for storage
bitSize EmptyPackNode = 0
bitSize (A16 b) = (V.length b) * 16
bitSize (S16 b) = (V.length b) * 16
bitSize (A64 b) = (V.length b) * 64
bitSize (S64 b) = (V.length b) * 64
bitSize (SInf b) = BV.size b

-- | Maybe bit size: for us all bits have a size and so returns a just object
bitSizeMaybe EmptyPackNode = Just 0
bitSizeMaybe (A16 b) = Just $ (V.length b) * 16
bitSizeMaybe (S16 b) = Just $ (V.length b) * 16
bitSizeMaybe (A64 b) = Just $ (V.length b) * 64
bitSizeMaybe (S64 b) = Just $ (V.length b) * 64
bitSizeMaybe (SInf b) = Just $ BV.size b

-- | all my types are unsigned so assigned is always False
isSigned _ = False 

-- | testBit function to return true if the ith bit is on
-- for multiple word structures it determines which word and then which bit based on value
testBit EmptyPackNode _ = False
testBit (A16 b) index = 
    let 
        myBit = div index 16
        myPos = rem index 16
    in B.testBit ( b V.! myBit) myPos
testBit (S16  b) index = 
    let 
        myBit = div index 16
        myPos = rem index 16
    in B.testBit ( b V.! myBit) myPos
testBit (A64  b) index = 
    let 
        myBit = div index 64
        myPos = rem index 64
    in B.testBit ( b V.! myBit) myPos
testBit (S64  b) index = 
    let 
        myBit = div index 64
        myPos = rem index 64
    in B.testBit ( b V.! myBit) myPos
testBit (SInf  b) index = B.testBit  b index

-- | The popCount uses the cardinality lookup table to return the number of bits turned on
-- no immediate way to put this on GPU's, but we can keep looking
popCount EmptyPackNode = 0
popCount (A16 b) = V.foldr (\bs acc -> acc + (cardinalityLookup bs cardTable)) 0  b
popCount (S16 b) = V.foldr (\bs acc -> acc + (cardinalityLookup bs cardTable)) 0  b
popCount (A64 b) = 
    let 
        numbits = V.length  b
        masked16 = V.map (\mask -> (.|.) (S64 $ V.replicate numbits mask) (A64 b)) allSelect
        shifts = trace ("masked 16 " ++ show masked16)
                    V.fromList [48, 32, 16, 0]
        maskedRight = V.zipWith (\s node -> shift node (-s)) shifts masked16
        convRight = V.map (\(A64 node) -> V.map (\bs -> fromIntegral bs :: Word16) node) maskedRight
        foldedCost = trace ("convRight " ++ show convRight)
                        V.foldr (\node acc -> acc + popCount (A16 node)) 0 convRight
    in foldedCost
popCount (S64 b) = 
    let 
        numbits = V.length b
        masked16 = V.map (\mask -> (.|.) (S64 $ V.replicate numbits mask) (S64 b)) allSelect
        shifts = trace ("masked 16 " ++ show masked16)
                    V.fromList [48, 32, 16, 0]
        maskedRight = V.zipWith (\s node -> shift node (-s)) shifts masked16
        convRight = V.map (\(S64 node) -> V.map (\bs -> fromIntegral bs :: Word16) node) maskedRight
        foldedCost =  trace ("convRight " ++ show convRight)
                        V.foldr (\node acc -> acc + popCount (S16 node)) 0 convRight
    in foldedCost
popCount (SInf b) = B.popCount b

-- | Function to shift by iterative block lengths and then fold together
-- Extremely useful in the Fitch packed algorithm
blockShiftAndFold :: String -> String -> BitPackedNode -> V.Vector Int -> Int -> BitPackedNode -> BitPackedNode
--blockShiftAndFold sideMode foldMode inbits blocklens alphlen initVal | trace ("block shift and fold " ++ show inbits) False = undefined
blockShiftAndFold sideMode foldMode (A16 inbits) blocklens _ (A16 initVal) = 
    let 
        c   | sideMode == "L" && foldMode == "&" = V.fromList $ zipWith3 (\b len iVal -> foldr (\s acc -> acc B..&. (B.shiftR b s)) iVal [1..len-1]) bitList lenList initList
            | sideMode == "R" && foldMode == "&" = V.fromList $ zipWith3 (\b len iVal -> foldr (\s acc -> acc B..&. (B.shiftR b s)) iVal [1..len-1]) bitList lenList initList 
            | sideMode == "L" && foldMode == "|" = V.fromList $ zipWith3 (\b len iVal -> foldr (\s acc -> acc B..|. (B.shiftL b s)) iVal [1..len-1]) bitList lenList initList 
            | sideMode == "R" && foldMode == "|" = V.fromList $ zipWith3 (\b len iVal -> foldr (\s acc -> acc B..|. (B.shiftR b s)) iVal [1..len-1]) bitList lenList initList 
            | otherwise = error "incorrect input for block shift and fold"
    in A16 c

    where
        bitList = V.toList inbits
        lenList = V.toList blocklens
        initList = V.toList initVal
blockShiftAndFold sideMode foldMode (A64 inbits) blocklens _ (A64 initVal) = 
    let 
        c   | sideMode == "L" && foldMode == "&" = V.fromList $ zipWith3 (\b len iVal -> foldr (\s acc -> acc B..&. (B.shiftR b s)) iVal [1..len-1]) bitList lenList initList
            | sideMode == "R" && foldMode == "&" = V.fromList $ zipWith3 (\b len iVal -> foldr (\s acc -> acc B..&. (B.shiftR b s)) iVal [1..len-1]) bitList lenList initList 
            | sideMode == "L" && foldMode == "|" = V.fromList $ zipWith3 (\b len iVal -> foldr (\s acc -> acc B..|. (B.shiftL b s)) iVal [1..len-1]) bitList lenList initList 
            | sideMode == "R" && foldMode == "|" = V.fromList $ zipWith3 (\b len iVal -> foldr (\s acc -> acc B..|. (B.shiftR b s)) iVal [1..len-1]) bitList lenList initList 
            | otherwise = error "incorrect input for block shift and fold"
    in A64 c

    where
        bitList = V.toList inbits
        lenList = V.toList blocklens
        initList = V.toList initVal
blockShiftAndFold sideMode foldMode (S16 inbits) _ alphlen (S16 initVal) = 
    let
        c   | sideMode == "L" && foldMode == "&" = V.zipWith (\b iVal -> foldr (\s acc -> acc B..&. (B.shiftL b s)) iVal [1..alphlen-1]) inbits initVal
            | sideMode == "R" && foldMode == "&" = V.zipWith (\b iVal -> foldr (\s acc -> acc B..&. (B.shiftR b s)) iVal [1..alphlen-1]) inbits initVal
            | sideMode == "L" && foldMode == "|" = V.zipWith (\b iVal -> foldr (\s acc -> acc B..|. (B.shiftL b s)) iVal [1..alphlen-1]) inbits initVal
            | sideMode == "R" && foldMode == "|" = V.zipWith (\b iVal -> foldr (\s acc -> acc B..|. (B.shiftR b s)) iVal [1..alphlen-1]) inbits initVal
            | otherwise = error "incorrect input for block shift and fold"
    in S16 c
blockShiftAndFold sideMode foldMode (S64 inbits) _ alphlen (S64 initVal) = 
    let
        c   | sideMode == "L" && foldMode == "&" = V.zipWith (\b iVal -> foldr (\s acc -> acc B..&. (B.shiftL b s)) iVal [1..alphlen-1]) inbits initVal
            | sideMode == "R" && foldMode == "&" = V.zipWith (\b iVal -> foldr (\s acc -> acc B..&. (B.shiftR b s)) iVal [1..alphlen-1]) inbits initVal
            | sideMode == "L" && foldMode == "|" = V.zipWith (\b iVal -> foldr (\s acc -> acc B..|. (B.shiftL b s)) iVal [1..alphlen-1]) inbits initVal
            | sideMode == "R" && foldMode == "|" = V.zipWith (\b iVal -> foldr (\s acc -> acc B..|. (B.shiftR b s)) iVal [1..alphlen-1]) inbits initVal
            | otherwise = error "incorrect input for block shift and fold"
    in S64 c
blockShiftAndFold sideMode foldMode (SInf inbits) _ alphlen (SInf initVal) = 
    let 
        c   | sideMode == "L" && foldMode == "&" = foldr (\s acc -> acc B..&. (B.shiftL inbits s)) initVal [1..alphlen-1]
            | sideMode == "R" && foldMode == "&" = foldr (\s acc -> acc B..&. (B.shiftR inbits s)) initVal [1..alphlen-1]
            | sideMode == "L" && foldMode == "|" = foldr (\s acc -> acc B..|. (B.shiftL inbits s)) initVal [1..alphlen-1]
            | sideMode == "R" && foldMode == "|" = foldr (\s acc -> acc B..|. (B.shiftR inbits s)) initVal [1..alphlen-1]
            | otherwise = error "incorrect input for block shift and fold"
    in SInf c
blockShiftAndFold _ _ _ _ _ _ = error "Attempt to block, shift, and fold nodes of two different types"

-- | Occupancy mask to remove excess bits from being counted
-- Bits should always be masked to achieve the correct cost from popCount
occupancyMask :: V.Vector Int -> V.Vector Int -> Int -> Int -> PackMode -> BitPackedNode
occupancyMask blockLens numCharVec alphLen numChars mode
    | bitLen mode == 16 && adaptive mode = A16 $ V.zipWith (\len nc -> B.shift (B.complement (0 :: Word16)) (16 - (len * nc))) blockLens numCharVec
    | bitLen mode == 64 && adaptive mode = A64 $ V.zipWith (\len nc -> B.shift (B.complement (0 :: Word64)) (64 - (len * nc))) blockLens numCharVec
    | bitLen mode == 16 && not (adaptive mode) = 
        let 
            numfit = div 16 alphLen
            needBits = (ceiling $ (fromIntegral numChars :: Float) / (fromIntegral numfit :: Float)) :: Int
            leftovers = (alphLen * numChars) - (alphLen * numfit * (needBits - 1))
            bitsFull = alphLen * numfit
            fullMask = B.shift (B.complement (0 :: Word16)) (16 - bitsFull)
            leftoverMask = --trace ("leftovers "++ show leftovers) 
                            B.shift (B.complement (0 :: Word16)) (16 - leftovers)
        in S16 $ V.replicate (needBits - 1) fullMask V.++ V.singleton leftoverMask
    | bitLen mode == 64 && not (adaptive mode) =
        let 
            numfit = div 64 alphLen
            needBits = (ceiling $ (fromIntegral numChars :: Float) / (fromIntegral numfit :: Float)) :: Int
            leftovers = (alphLen * numChars) - (alphLen * numfit * (needBits - 1))
            bitsFull = alphLen * numfit
            fullMask = B.shift (B.complement (0 :: Word64)) (64 - bitsFull)
            leftoverMask = B.shift (B.complement (0 :: Word64)) (64 - leftovers)
        in S64 $ V.replicate (needBits - 1) fullMask V.++ V.singleton leftoverMask 
    | bitLen mode == 0 = SInf $ BV.fromBits $ replicate (alphLen * numChars) True
    | otherwise = error "incorrect mode for for occupancy mask"

-- | Standard mask where every alphlen bit is turned on
-- automatically determines the periodicity
standardMask :: V.Vector Int -> BitPackedNode -> Int -> PackMode -> BitPackedNode
--standardMask blockLens occMask alphLen mode | trace ("standard mask") False = undefined
standardMask blockLens occMask alphLen mode 
    | bitLen mode == 16 && adaptive mode && V.null blockLens = A16 V.empty
    | bitLen mode == 16 && adaptive mode = 
        let         
            b = V.head blockLens
            positions = [x | x <- [0..15], (16 - x) `mod` b == 0]
            standard = foldr (\i acc -> 2^i + acc) (0 :: Word16) positions
            (A16 mask) = occMask
            maskStandard = standard B..&. (V.head mask)
            (A16 restMask) = standardMask (V.tail blockLens) (A16 $ V.tail mask) alphLen mode
        in A16 $ maskStandard `V.cons` restMask
    | bitLen mode == 64 && adaptive mode  && V.null blockLens = A64 V.empty
    | bitLen mode == 64 && adaptive mode = 
        let         
            b = V.head blockLens
            positions = [x | x <- [0..63], (64 - x) `mod` b == 0]
            standard = foldr (\i acc -> 2^i + acc) (0 :: Word64) positions
            (A64 mask) = occMask
            maskStandard = standard B..&. (V.head mask)
            (A64 restMask) = standardMask (V.tail blockLens) (A64 $ V.tail mask) alphLen mode
        in A64 $ maskStandard `V.cons` restMask
    | bitLen mode == 16 && not (adaptive mode) = 
        let 
            positions = [x | x <- [0..15], (16 - x) `mod` alphLen == 0]
            standard = foldr (\i acc -> 2^i + acc) (0 :: Word16) positions
            (S16 mask) = occMask
        in S16 $ V.map (\m -> standard B..&. m) mask
    | bitLen mode == 64 && not (adaptive mode) =
        let 
            positions = [x | x <- [0..63], (64 - x) `mod` alphLen == 0]
            standard = foldr (\i acc -> 2^i + acc) (0 :: Word64) positions
            (S64 mask) = occMask
        in S64 $ V.map (\m -> standard B..&. m) mask
    | bitLen mode == 0 = 
        let 
            numChars = fromJust $ bitSizeMaybe occMask
            numReps = div numChars alphLen
            vov = replicate numReps ((replicate (alphLen-1) False) ++ [True]) 
        in SInf $ BV.fromBits $ foldr (\v acc -> acc ++ v) [] vov
    | otherwise = error "incorrect mode for for occupancy mask"

-- | Unified function to get both the occupancy and standard masks
genMasks :: V.Vector Int -> V.Vector Int -> Int -> Int -> PackMode -> (BitPackedNode, BitPackedNode)
genMasks blockLens numCharVec alphLen numChars mode = 
    let
        occMask = occupancyMask blockLens numCharVec alphLen numChars mode
        sMask = standardMask blockLens occMask alphLen mode
    in (occMask, sMask)

-- | Display function that prints a node's meaning given the packed version and info
displayNode :: BitPackedNode -> V.Vector (V.Vector [Char]) -> [Char] -> V.Vector Int -> V.Vector [Int] -> String
displayNode EmptyPackNode _ _ _ _ = ""
displayNode (A16 node) bitAlphs _ blockLens shuffles = 
    let 
        remixed = V.zipWith3 (\b len alphs -> displayBit (Left b) len alphs) node blockLens bitAlphs
        mapRemix = V.toList $ V.zipWith (\indices strs -> zip indices strs) shuffles remixed
        unrollMap = foldr (\rList acc -> rList ++ acc) [] mapRemix
        replace = V.replicate (length unrollMap) "" 
        ordered = V.toList $ (V.//) replace unrollMap
    in foldr (\str acc -> str ++ acc) [] ordered
displayNode (S16 node) _ oneAlph _ _ = 
    let 
        bitlen = length oneAlph
        numfit = div 16 bitlen
        alphabets = V.replicate numfit oneAlph    
        strs = V.foldl (\acc b -> acc ++ displayBit (Left b) bitlen alphabets) [] node
    in foldr (\str acc -> str ++ acc) "" strs
displayNode (A64 node) bitAlphs _ blockLens shuffles = 
    let 
        remixed = V.zipWith3 (\b len alphs -> displayBit (Right b) len alphs) node blockLens bitAlphs
        mapRemix = V.toList $ V.zipWith (\indices strs -> zip indices strs) shuffles remixed
        unrollMap = foldr (\rList acc -> rList ++ acc) [] mapRemix
        replace = V.replicate (length unrollMap) "" 
        ordered = V.toList $ (V.//) replace unrollMap
    in foldr (\str acc -> str ++ acc) [] ordered
displayNode (S64 node) _ oneAlph _ _ = 
    let 
        bitlen = length oneAlph
        numfit = div 64 bitlen
        alphabets = V.replicate numfit oneAlph    
        strs = V.foldl (\acc b -> acc ++ displayBit (Right b) bitlen alphabets) [] node
    in foldr (\str acc -> str ++ acc) "" strs
displayNode (SInf node) _ oneAlph _ _ = 
    let 
        alphlen = length oneAlph
        indices = [0..(fromJust $ B.bitSizeMaybe node) - 1]
        numfit = div (fromJust $ B.bitSizeMaybe node) alphlen
        alphpos = foldr (\acc s -> acc ++ s) [] (replicate numfit [0..(alphlen-1)])
    in (foldl (\acc i -> if (B.testBit node i) then acc ++ [(oneAlph !! (alphpos !! i))] else acc) "" indices)

-- | Display a single word of given length
displayBit :: Either Word16 Word64 -> Int -> V.Vector [Char] -> [String]
displayBit eWord bitlen alphabets 
    | isLeft eWord = 
        let 
            b = head $ lefts [eWord]
            numchars = V.length alphabets
            indices = V.fromList [0, bitlen..bitlen*numchars]
            alphpos = [0..bitlen-1]
        in V.toList $ V.zipWith (\alph lIndex -> (foldr (\a acc -> if (B.testBit b (15 - a - lIndex)) then acc ++ [(alph !! a)] else acc) "|" alphpos) ) alphabets indices
    | otherwise = 
        let 
            b = head $ rights [eWord]
            numchars = V.length alphabets
            indices = V.fromList [0, bitlen..bitlen*numchars]
            alphpos = [0..bitlen-1]
        in V.toList $ V.zipWith (\alph lIndex -> (foldr (\a acc -> if (B.testBit b (63 - a - lIndex)) then acc ++ [(alph !! a)] else acc) "|" alphpos) ) alphabets indices


-- | Get the cost, intelligently figuring out whether it's adaptive or non-adaptive
-- this function allows for the cost to be divided by the alphabet length at each position
getNodeCost :: BitPackedNode -> PackMode -> V.Vector Int -> Int -> Float
--getNodeCost node mode blockLens alphLen | trace ("cost of node " ++ show node) False = undefined
getNodeCost node mode blockLens alphLen
    | not $ adaptive mode = fromIntegral $ div (popCount node) alphLen  
    | bitLen mode == 16 = 
        let 
            (A16 bits) = node
            counts = V.map (\b -> cardinalityLookup b cardTable) bits
            costs = V.zipWith (\l c -> div c l) blockLens counts
        in fromIntegral $ V.sum costs
    | otherwise =         
        let 
            numbits = div (fromJust $ bitSizeMaybe node) 64
            masked16 = trace ("masked 16 " ++ show numbits)
                            V.map (\mask -> (.|.) (A64 $ V.replicate numbits mask) node) allSelect
            shifts = V.fromList [48, 32, 16, 0]
            maskedRight = V.zipWith (\s n -> shift n (-s)) shifts masked16
            convRight = trace ("convRight " ++ show maskedRight)
                            V.map (\(A64 n) -> V.map (\b -> fromIntegral b :: Word16) n) maskedRight
            counts = V.map (\n -> popCount (A16 n)) convRight 
            costs = V.zipWith (\l c -> div c l) blockLens counts
        in fromIntegral $ V.sum costs




