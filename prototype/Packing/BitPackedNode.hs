{-# LANGUAGE BangPatterns, DeriveGeneric #-}
module Packing.BitPackedNode (BitPackedNode (..), 
                        PackMode (bitLen, adaptive, MakePackMode), 
                        (.&.), 
                        (.|.), 
                        complement, 
                        popCount, 
                        makeNode, 
                        blockShiftAndFold, 
                        genMasks, 
                        displayNode,
                        getNodeCost,
                        shift,
                        bitSize) where

import           Control.DeepSeq
import           Data.Bits 
import qualified Data.BitVector as BV
import           Data.Either
import           Data.List
import           Data.Maybe
import qualified Data.Vector    as V
import           Data.Word
import qualified Data.Map       as M
import           Debug.Trace
import           GHC.Generics
import           Packing.CardinalityLookup

-- | Define the data type: 
-- 1) all data is stored in vectors
-- 2) bits can be packed adaptively (based on the part of the alphabet used for each character) or statically (based on the overall alphabet)
-- 3) All data types are words to avoid sign issues, and can be of length 16 or 64
data BitPackedNode = EmptyPackNode | A16 (V.Vector Word16) | S16 (V.Vector Word16) | A64 (V.Vector Word64) | S64 (V.Vector Word64) | SInf BV.BitVector deriving (Eq, Generic, Show)

instance NFData BitPackedNode
instance NFData BV.BV where
    rnf bv = (\ !_ -> ()) bv
instance NFData PackMode

-- | make the cardinality table and the masks for 64 bit cardinality
-- All cardinalities except for the "infinite" type are from the stored table
cardTable :: V.Vector Int
cardTable = makeLookup 

fth16, thd16, snd16, fst16 :: Word64
fth16 = (foldr (\i acc -> acc + 2^i) (0 :: Word64) ([48 .. 64] :: [Int])) 
thd16 = (foldr (\i acc -> acc + 2^i) (0 :: Word64) ([32 .. 47] :: [Int])) 
snd16 = (foldr (\i acc -> acc + 2^i) (0 :: Word64) ([16 .. 31] :: [Int])) 
fst16 = (foldr (\i acc -> acc + 2^i) (0 :: Word64) ([0 .. 15] :: [Int]))

allSelect :: V.Vector Word64
allSelect = V.fromList [fst16, snd16, thd16, fth16] 

-- | Information data type for the pack mode
data PackMode = MakePackMode     { bitLen :: Int
                                , adaptive :: Bool
                                } deriving (Show, Eq, Generic)

-- | Make the instance
instance Bits BitPackedNode where
    -- | And function: throws errors for different length bits, and of any empty node returns an empty
    (.&.) EmptyPackNode _ = EmptyPackNode
    (.&.) _ EmptyPackNode = EmptyPackNode
    (.&.) (A16 bit1) (A16 bit2) = 
        let 
            a   | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
                | otherwise = V.zipWith (\b1 b2 -> (.&.) b1 b2) bit1 bit2
        in A16 a
    (.&.) (S16 bit1) (S16 bit2) = 
        let 
            a   | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
                | otherwise = V.zipWith (\b1 b2 -> (.&.) b1 b2) bit1 bit2
        in S16 a
    (.&.) (A64 bit1) (A64 bit2) = 
        let 
            a   | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
                | otherwise = V.zipWith (\b1 b2 -> (.&.) b1 b2) bit1 bit2
        in A64 a
    (.&.) (S64 bit1) (S64 bit2) = 
        let 
            a     | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
                | otherwise = V.zipWith (\b1 b2 -> (.&.) b1 b2) bit1 bit2
        in S64 a
    (.&.) (SInf bit1) (SInf bit2) = 
        let
            a   | (BV.size bit1) /= (BV.size bit2) = error "Attempt to take and of bits of different lengths"
                | otherwise = (.&.) bit1 bit2
        in SInf a
    (.&.) _ _ = error "Attempt to take and of two different node types"

    -- | Or function: throws errors for different length bits, and of any empty node returns an empty
    (.|.) EmptyPackNode _ = EmptyPackNode
    (.|.) _ EmptyPackNode = EmptyPackNode
    (.|.) (A16 bit1) (A16 bit2) = 
        let 
            a   | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
                | otherwise = V.zipWith (\b1 b2 -> (.|.) b1 b2) bit1 bit2
        in A16 a
    (.|.) (S16 bit1) (S16 bit2) = 
        let 
            a   | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
                | otherwise = V.zipWith (\b1 b2 -> (.|.) b1 b2) bit1 bit2
        in S16 a
    (.|.) (A64 bit1) (A64 bit2) = 
        let 
            a   | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
                | otherwise = V.zipWith (\b1 b2 -> (.|.) b1 b2) bit1 bit2
        in A64 a
    (.|.) (S64 bit1) (S64 bit2) = 
        let 
            a   | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
                | otherwise = V.zipWith (\b1 b2 -> (.|.) b1 b2) bit1 bit2
        in S64 a
    (.|.) (SInf bit1) (SInf bit2) = 
        let
            a   | (BV.size bit1) /= (BV.size bit2) = error "Attempt to take and of bits of different lengths"
                | otherwise = (.|.) bit1 bit2
        in SInf a
    (.|.) _ _ = error "Attempt to take or of two different node types"

    -- | xor function as a combination of and, or
    xor EmptyPackNode _ = EmptyPackNode
    xor _ EmptyPackNode = EmptyPackNode
    xor bit1 bit2 = (.&.) ((.|.) bit1 bit2) (complement ((.&.) bit1 bit2))

    -- | complement function turning on opposite bits
    complement EmptyPackNode = EmptyPackNode
    complement (A16 bita16) = A16 $ V.map (\b -> complement b) bita16
    complement (S16 bits16) = S16 $ V.map (\b -> complement b) bits16
    complement (A64 bita64) = A64 $ V.map (\b -> complement b) bita64
    complement (S64 bits64) = S64 $ V.map (\b -> complement b) bits64
    complement (SInf biti) = SInf $ complement biti

    -- | Generic shift function, a negative value causes a right shift and a positive a right shift
    shift EmptyPackNode _ = EmptyPackNode
    shift (A16 bita16) s = A16 $ V.map (\b -> shift b s) bita16
    shift (S16 bits16) s = S16 $ V.map (\b -> shift b s) bits16
    shift (A64 bita64) s = A64 $ V.map (\b -> shift b s) bita64
    shift (S64 bits64) s = S64 $ V.map (\b -> shift b s) bits64
    shift (SInf biti) s = SInf $ shift biti s

    -- | Rotate function for bits
    rotate EmptyPackNode _ = EmptyPackNode
    rotate (A16 bita16) r = A16 $ V.map (\b -> rotate b r) bita16
    rotate (S16 bits16) r = S16 $ V.map (\b -> rotate b r) bits16
    rotate (A64 bita64) r = A64 $ V.map (\b -> rotate b r) bita64
    rotate (S64 bits64) r = S64 $ V.map (\b -> rotate b r) bits64
    rotate (SInf biti) r = SInf $ rotate biti r

    -- | Both setBit and bit i are not meaningful for these data structures (could be a variety of things) 
    -- and so simply return an empty
    -- NOTE THAT setBIT IS NOT MEANINGFUL FOR THIS DATA STRUCTURE
    setBit _ _ = EmptyPackNode

    bit _ = EmptyPackNode -- this is not meaningful for the bit vectors so just always give back an empty

    -- | The bit size is the number of slots for storage
    bitSize EmptyPackNode = 0
    bitSize (A16 bita16) = (V.length bita16) * 16
    bitSize (S16 bits16) = (V.length bits16) * 16
    bitSize (A64 bita64) = (V.length bita64) * 64
    bitSize (S64 bits64) = (V.length bits64) * 64
    bitSize (SInf biti) = BV.size biti

    -- | Maybe bit size: for us all bits have a size and so returns a just object
    bitSizeMaybe EmptyPackNode = Just 0
    bitSizeMaybe (A16 bita16) = Just $ (V.length bita16) * 16
    bitSizeMaybe (S16 bits16) = Just $ (V.length bits16) * 16
    bitSizeMaybe (A64 bita64) = Just $ (V.length bita64) * 64
    bitSizeMaybe (S64 bits64) = Just $ (V.length bits64) * 64
    bitSizeMaybe (SInf biti) = Just $ BV.size biti

    -- | all my types are unsigned so assigned is always False
    assigned _ = False 

    -- | testBit function to return true if the ith bit is on
    -- for multiple word structures it determines which word and then which bit based on value
    testBit EmptyPackNode _ = False
    testBit (A16 bita16) index = 
        let 
            myBit = div index 16
            myPos = rem index 16
        in testBit (bita16 V.! myBit) myPos
    testBit (S16 bits16) index = 
        let 
            myBit = div index 16
            myPos = rem index 16
        in testBit (bits16 V.! myBit) myPos
    testBit (A64 bita64) index = 
        let 
            myBit = div index 64
            myPos = rem index 64
        in testBit (bita64 V.! myBit) myPos
    testBit (S64 bits64) index = 
        let 
            myBit = div index 64
            myPos = rem index 64
        in testBit (bits64 V.! myBit) myPos
    testBit (SInf biti) index = testBit biti index

    -- | The popCount uses the cardinality lookup table to return the number of bits turned on
    popCount EmptyPackNode = 0
    popCount (A16 bita16) = V.foldr (\b acc -> acc + (cardinalityLookup b cardTable)) 0 bita16
    popCount (S16 bits16) = V.foldr (\b acc -> acc + (cardinalityLookup b cardTable)) 0 bits16
    popCount (A64 bita64) = 
        let 
            numbits = V.length bita64
            masked16 = V.map (\mask -> (.|.) (A64 $ V.replicate numbits mask) (A64 bita64)) allSelect
            shifts = V.fromList [48, 32, 16, 0]
            maskedRight = V.zipWith (\s node -> shift node (-s)) shifts masked16
            convRight = V.map (\(A64 node) -> V.map (\b -> fromIntegral b :: Word16) node) maskedRight
            foldedCost = V.foldr (\node acc -> acc + popCount (A16 node)) 0 convRight
        in foldedCost
    popCount (S64 bits64) = 
        let 
            numbits = V.length bits64
            masked16 = V.map (\mask -> (.|.) (S64 $ V.replicate numbits mask) (S64 bits64)) allSelect
            shifts = --trace ("masked 16 " ++ show masked16)
                        V.fromList [48, 32, 16, 0]
            maskedRight = V.zipWith (\s node -> shift node (-s)) shifts masked16
            convRight = V.map (\(S64 node) -> V.map (\b -> fromIntegral b :: Word16) node) maskedRight
            foldedCost =  --trace ("convRight " ++ show convRight)
                            V.foldr (\node acc -> acc + popCount (S16 node)) 0 convRight
        in foldedCost
    popCount (SInf biti) = popCount biti

-- | Creation of an encoded node given the string and relevant information
makeNode :: String -> M.Map Char String -> V.Vector(V.Vector [Char]) -> String -> V.Vector [Int] -> PackMode -> BitPackedNode
--makeNode string special bitAlphs overallAlph shuffles mode | trace ("makeNode with " ++ show mode) False = undefined
makeNode string special bitAlphs overallAlph shuffles mode 
    | bitLen mode == 16 && adaptive mode = A16 $ V.zipWith (\alph chars -> makeBit16 chars special alph overallAlph mode) bitAlphs remixString

    | bitLen mode == 16 && not (adaptive mode) && (null string) = S16 V.empty
    | bitLen mode == 16 && not (adaptive mode) =  
        let 
            numfit = div 16 (length overallAlph)
            firstbit = take numfit string
            tailbits = drop numfit string
            (S16 tailOut) = makeNode tailbits special bitAlphs overallAlph shuffles mode
            outVec = (V.singleton (makeBit16 firstbit special (V.head bitAlphs) overallAlph mode)) V.++ tailOut
        in S16 $ outVec
    | bitLen mode == 64 && adaptive mode = A64 $ V.zipWith (\alph chars -> makeBit64 chars special alph overallAlph mode) bitAlphs remixString

    | bitLen mode == 64 && not (adaptive mode) && (null string) =  S64 V.empty
    | bitLen mode == 64 && not (adaptive mode) = 
        let
            numfit = div 64 (length overallAlph)
            firstbit = take numfit string
            tailbits = drop  numfit string
            (S64 tailOut) = makeNode tailbits special bitAlphs overallAlph shuffles mode
            outVec = (V.singleton (makeBit64 firstbit special (V.head bitAlphs) overallAlph mode)) V.++ tailOut
        in S64 $ outVec

    | bitLen mode == 0 = 
        let 
            specialKeys = M.keys special
            recodeS = foldr (\c acc -> if c `elem` specialKeys then (special M.! c) : acc else [c] : acc) [] string
            tfVec = foldr (\str acc -> (map (\a -> if a `elem` str then True else False) overallAlph) ++ acc) [] recodeS
            outvec = BV.fromBits tfVec
        in --trace ("outvec for inf " ++ show outvec)
            SInf outvec
    | otherwise = error "incorrect packing mode, cannot create node "
        where remixString = V.map (\indices -> (map (\i -> string !! i) indices)) shuffles

-- | Inner work for the creation of a single encoded word16
makeBit16 :: String -> M.Map Char String -> V.Vector String -> String -> PackMode -> Word16
--makeBit16 string special alphabets overallAlph mode | trace ("makeBit16 " ++ show string ++ " and alphs " ++ show overallAlph) False = undefined
makeBit16 string special alphabets overallAlph mode
    | (bitLen mode == 16) && (adaptive mode) && (False `V.elem` charCheck) = error "Error in makeBit16, character not in its corresponding alphabet"
    | (bitLen mode == 16) && (adaptive mode) =
        let 
            blocklen = length $ V.head alphabets 
            indices = V.fromList [0..(V.length convstr)-1]
            setbits = V.zipWith3 (\i str alph -> foldr (\char acc -> (.|.) acc (setBit 0 (15 - (i*blocklen) - (fromJust $ elemIndex char alph)))) (0 :: Word16) str) indices recodeS alphabets 
        in (V.foldr (\b acc -> (.|.) acc b) (0 :: Word16) setbits)
    | bitLen mode == 16 && not (adaptive mode) && (False `V.elem` charCheckSingle) = error ("Error in makeBit16, character " ++ show convstr ++ " not in the alphabet " ++ show overallAlph)
    | bitLen mode == 16 && not (adaptive mode) = --trace "not adaptive" $
        let 
            blocklen = length overallAlph
            setbits = V.imap (\i str -> foldr (\char acc -> (.|.) acc (setBit 0 (15 - (i*blocklen) - (fromJust $ elemIndex char overallAlph)))) (0 :: Word16) str) recodeS
        in (V.foldr (\b acc -> (.|.) acc b) (0 :: Word16) setbits)
    | otherwise = error "incorrect packing mode, cannot create node "
        where 
            convstr = V.fromList string
            specialKeys = M.keys special
            recodeS = V.foldr (\c acc -> if c `elem` specialKeys then (special M.! c) `V.cons` acc else [c] `V.cons` acc) V.empty convstr
            charCheck = V.zipWith (\str alph -> foldr (\char acc -> if char `elem` alph then acc else False) True str) recodeS alphabets
            charCheckSingle = V.map (\str -> foldr (\char acc -> if char `elem` overallAlph then acc else False) True str) recodeS

-- | Inner work to make an encoded word64 
makeBit64 :: String -> M.Map Char String -> V.Vector String -> String -> PackMode -> Word64
makeBit64 string special alphabets overallAlph mode
    | (bitLen mode == 64) && (adaptive mode) && (False `V.elem` charCheck) = error "Error in makeBit16, character not in its corresponding alphabet"
    | bitLen mode == 64 && adaptive mode = 
        let 
            blocklen = length $ V.head alphabets 
            indices = V.fromList [0..(V.length convstr)-1]
            setbits = V.zipWith3 (\i str alph -> foldr (\char acc -> (.|.) acc (setBit 0 (63 - (i*blocklen) - (fromJust $ elemIndex char alph)))) (0 :: Word64) str) indices recodeS alphabets 
        in (V.foldr (\b acc -> (.|.) acc b) (0 :: Word64) setbits)
    | bitLen mode == 64 && not (adaptive mode) && (False `V.elem` charCheckSingle) = error ("Error in makeBit16, character " ++ show convstr ++ " not in the alphabet " ++ show overallAlph)
    | bitLen mode == 64 && not (adaptive mode) = 
        let 
            blocklen = length overallAlph
            setbits = V.imap (\i str -> foldr (\char acc -> (.|.) acc (setBit 0 (63 - (i*blocklen) - (fromJust $ elemIndex char overallAlph)))) (0 :: Word64) str) recodeS
        in (V.foldr (\b acc -> (.|.) acc b) (0 :: Word64) setbits)
    | otherwise = error "incorrect packing mode, cannot create node "
        where 
            convstr = V.fromList string
            specialKeys = M.keys special
            recodeS = V.foldr (\c acc -> if c `elem` specialKeys then (special M.! c) `V.cons` acc else [c] `V.cons` acc) V.empty convstr
            charCheck = V.zipWith (\str alph -> foldr (\char acc -> if char `elem` alph then acc else False) True str) recodeS alphabets
            charCheckSingle = V.map (\str -> foldr (\char acc -> if char `elem` overallAlph then acc else False) True str) recodeS

-- | Function to shift by iterative block lengths and then fold together
-- Extremely useful in the Fitch packed algorithm
blockShiftAndFold :: String -> String -> BitPackedNode -> V.Vector Int -> Int -> BitPackedNode -> BitPackedNode
--blockShiftAndFold sideMode foldMode inbits blocklens alphlen initVal | trace ("block shift and fold " ++ show inbits) False = undefined
blockShiftAndFold sideMode foldMode (A16 inbits) blocklens _ (A16 initVal) = 
    let 
        c     | sideMode == "L" && foldMode == "&" = V.zipWith3 (\b len iVal -> foldr (\s acc -> (.&.) acc (shiftL b s)) iVal [1..len-1]) inbits blocklens initVal
            | sideMode == "R" && foldMode == "&" = V.zipWith3 (\b len iVal -> foldr (\s acc -> (.&.) acc (shiftR b s)) iVal [1..len-1]) inbits blocklens initVal
            | sideMode == "L" && foldMode == "|" = V.zipWith3 (\b len iVal -> foldr (\s acc -> (.|.) acc (shiftL b s)) iVal [1..len-1]) inbits blocklens initVal
            | sideMode == "R" && foldMode == "|" = V.zipWith3 (\b len iVal -> foldr (\s acc -> (.|.) acc (shiftR b s)) iVal [1..len-1]) inbits blocklens initVal
            | otherwise = error "incorrect input for block shift and fold"
    in A16 c
blockShiftAndFold sideMode foldMode (A64 inbits) blocklens _ (A64 initVal) = 
    let 
        c     | sideMode == "L" && foldMode == "&" = V.zipWith3 (\b len iVal -> foldr (\s acc -> (.&.) acc (shiftL b s)) iVal [1..len-1]) inbits blocklens initVal
            | sideMode == "R" && foldMode == "&" = V.zipWith3 (\b len iVal -> foldr (\s acc -> (.&.) acc (shiftR b s)) iVal [1..len-1]) inbits blocklens initVal
            | sideMode == "L" && foldMode == "|" = V.zipWith3 (\b len iVal -> foldr (\s acc -> (.|.) acc (shiftL b s)) iVal [1..len-1]) inbits blocklens initVal
            | sideMode == "R" && foldMode == "|" = V.zipWith3 (\b len iVal -> foldr (\s acc -> (.|.) acc (shiftR b s)) iVal [1..len-1]) inbits blocklens initVal
            | otherwise = error "incorrect input for block shift and fold"
    in A64 c
blockShiftAndFold sideMode foldMode (S16 inbits) _ alphlen (S16 initVal) = 
    let
        c     | sideMode == "L" && foldMode == "&" = V.zipWith (\b iVal -> foldr (\s acc -> (.&.) acc (shiftL b s)) iVal [1..alphlen-1]) inbits initVal
            | sideMode == "R" && foldMode == "&" = V.zipWith (\b iVal -> foldr (\s acc -> (.&.) acc (shiftR b s)) iVal [1..alphlen-1]) inbits initVal
            | sideMode == "L" && foldMode == "|" = V.zipWith (\b iVal -> foldr (\s acc -> (.|.) acc (shiftL b s)) iVal [1..alphlen-1]) inbits initVal
            | sideMode == "R" && foldMode == "|" = V.zipWith (\b iVal -> foldr (\s acc -> (.|.) acc (shiftR b s)) iVal [1..alphlen-1]) inbits initVal
            | otherwise = error "incorrect input for block shift and fold"
    in S16 c
blockShiftAndFold sideMode foldMode (S64 inbits) _ alphlen (S64 initVal) = 
    let
        c     | sideMode == "L" && foldMode == "&" = V.zipWith (\b iVal -> foldr (\s acc -> (.&.) acc (shiftL b s)) iVal [1..alphlen-1]) inbits initVal
            | sideMode == "R" && foldMode == "&" = V.zipWith (\b iVal -> foldr (\s acc -> (.&.) acc (shiftR b s)) iVal [1..alphlen-1]) inbits initVal
            | sideMode == "L" && foldMode == "|" = V.zipWith (\b iVal -> foldr (\s acc -> (.|.) acc (shiftL b s)) iVal [1..alphlen-1]) inbits initVal
            | sideMode == "R" && foldMode == "|" = V.zipWith (\b iVal -> foldr (\s acc -> (.|.) acc (shiftR b s)) iVal [1..alphlen-1]) inbits initVal
            | otherwise = error "incorrect input for block shift and fold"
    in S64 c
blockShiftAndFold sideMode foldMode (SInf inbits) _ alphlen (SInf initVal) = 
    let 
        c     | sideMode == "L" && foldMode == "&" = foldr (\s acc -> (.&.) acc (shiftL inbits s)) initVal [1..alphlen-1]
            | sideMode == "R" && foldMode == "&" = foldr (\s acc -> (.&.) acc (shiftR inbits s)) initVal [1..alphlen-1]
            | sideMode == "L" && foldMode == "|" = foldr (\s acc -> (.|.) acc (shiftL inbits s)) initVal [1..alphlen-1]
            | sideMode == "R" && foldMode == "|" = foldr (\s acc -> (.|.) acc (shiftR inbits s)) initVal [1..alphlen-1]
            | otherwise = error "incorrect input for block shift and fold"
    in SInf c
blockShiftAndFold _ _ EmptyPackNode _ _ _ = error "Attempt to block, shift, and fold empty node"
blockShiftAndFold _ _ node _ _ initNode = error ("Attempt to block, shift, and fold node " ++ show node ++ " and initial " ++ show initNode ++ " of two different types")

-- | Occupancy mask to remove excess bits from being counted
-- Bits should always be masked to achieve the correct cost from popCount
occupancyMask :: V.Vector Int -> V.Vector Int -> Int -> Int -> PackMode -> BitPackedNode
occupancyMask blockLens numCharVec alphLen numChars mode
    | bitLen mode == 16 && adaptive mode = A16 $ V.zipWith (\len nc -> shift (complement (0 :: Word16)) (16 - (len * nc))) blockLens numCharVec
    | bitLen mode == 64 && adaptive mode = A64 $ V.zipWith (\len nc -> shift (complement (0 :: Word64)) (64 - (len * nc))) blockLens numCharVec
    | bitLen mode == 16 && not (adaptive mode) = 
        let 
            numfit = div 16 alphLen
            needBits = (ceiling $ (fromIntegral numChars :: Float) / (fromIntegral numfit :: Float)) :: Int
            leftovers = (alphLen * numChars) - (alphLen * numfit * (needBits - 1))
            bitsFull = alphLen * numfit
            fullMask = shift (complement (0 :: Word16)) (16 - bitsFull)
            leftoverMask = --trace ("leftovers "++ show leftovers) 
                            shift (complement (0 :: Word16)) (16 - leftovers)
        in S16 $ V.replicate (needBits - 1) fullMask V.++ V.singleton leftoverMask
    | bitLen mode == 64 && not (adaptive mode) =
        let 
            numfit = div 64 alphLen
            needBits = (ceiling $ (fromIntegral numChars :: Float) / (fromIntegral numfit :: Float)) :: Int
            leftovers = (alphLen * numChars) - (alphLen * numfit * (needBits - 1))
            bitsFull = alphLen * numfit
            fullMask = shift (complement (0 :: Word64)) (64 - bitsFull)
            leftoverMask = shift (complement (0 :: Word64)) (64 - leftovers)
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
            maskStandard = (.&.) standard (V.head mask)
            (A16 restMask) = standardMask (V.tail blockLens) (A16 $ V.tail mask) alphLen mode
        in A16 $ maskStandard `V.cons` restMask
    | bitLen mode == 64 && adaptive mode  && V.null blockLens = A64 V.empty
    | bitLen mode == 64 && adaptive mode = 
        let         
            b = V.head blockLens
            positions = [x | x <- [0..63], (64 - x) `mod` b == 0]
            standard = foldr (\i acc -> 2^i + acc) (0 :: Word64) positions
            (A64 mask) = occMask
            maskStandard = (.&.) standard (V.head mask)
            (A64 restMask) = standardMask (V.tail blockLens) (A64 $ V.tail mask) alphLen mode
        in A64 $ maskStandard `V.cons` restMask
    | bitLen mode == 16 && not (adaptive mode) = 
        let 
            positions = [x | x <- [0..15], (16 - x) `mod` alphLen == 0]
            standard = foldr (\i acc -> 2^i + acc) (0 :: Word16) positions
            (S16 mask) = occMask
        in S16 $ V.map (\m -> (.&.) standard m) mask
    | bitLen mode == 64 && not (adaptive mode) =
        let 
            positions = [x | x <- [0..63], (64 - x) `mod` alphLen == 0]
            standard = foldr (\i acc -> 2^i + acc) (0 :: Word64) positions
            (S64 mask) = occMask
        in S64 $ V.map (\m -> (.&.) standard m) mask
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
        indices = [0..(fromJust $ bitSizeMaybe node) - 1]
        numfit = div (fromJust $ bitSizeMaybe node) alphlen
        alphpos = foldr (\acc s -> acc ++ s) [] (replicate numfit [0..(alphlen-1)])
    in (foldl (\acc i -> if (testBit node i) then acc ++ [(oneAlph !! (alphpos !! i))] else acc) "" indices)

-- | Display a single word of given length
displayBit :: Either Word16 Word64 -> Int -> V.Vector [Char] -> [String]
displayBit eWord bitlen alphabets 
    | isLeft eWord = 
        let 
            b = head $ lefts [eWord]
            numchars = V.length alphabets
            indices = V.fromList [0, bitlen..bitlen*numchars]
            alphpos = [0..bitlen-1]
        in V.toList $ V.zipWith (\alph lIndex -> (foldr (\a acc -> if (testBit b (15 - a - lIndex)) then acc ++ [(alph !! a)] else acc) "|" alphpos) ) alphabets indices
    | otherwise = 
        let 
            b = head $ rights [eWord]
            numchars = V.length alphabets
            indices = V.fromList [0, bitlen..bitlen*numchars]
            alphpos = [0..bitlen-1]
        in V.toList $ V.zipWith (\alph lIndex -> (foldr (\a acc -> if (testBit b (63 - a - lIndex)) then acc ++ [(alph !! a)] else acc) "|" alphpos) ) alphabets indices


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
            masked16 = --trace ("masked 16 " ++ show numbits)
                            V.map (\mask -> (.|.) (A64 $ V.replicate numbits mask) node) allSelect
            shifts = V.fromList [48, 32, 16, 0]
            maskedRight = V.zipWith (\s n -> shift n (-s)) shifts masked16
            convRight = --trace ("convRight " ++ show maskedRight)
                            V.map (\(A64 n) -> V.map (\b -> fromIntegral b :: Word16) n) maskedRight
            counts = V.map (\n -> popCount (A16 n)) convRight 
            costs = V.zipWith (\l c -> div c l) blockLens counts
        in fromIntegral $ V.sum costs




