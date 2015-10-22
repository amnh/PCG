{-# LANGUAGE BangPatterns, DeriveGeneric #-}
module PackingPar.ParEnabledNode2 (ParEnabledNode (..), 
                        (.&.),
                        ParMode (..),
                        ParPackMode (..),
                        (.|.),
                        complement, 
                        popCount, 
                        makeNode, 
                        blockShiftAndFold, 
                        genMasks, 
                        getNodeCost,
                        shift,
                        bitSize) where

-- | imports
import           Control.DeepSeq
import  Data.Bits
import           Data.Either
import           Data.List
import           Data.Maybe
import qualified Data.Vector    as V
import           Data.Word
import qualified Data.Map       as M
import           Debug.Trace
import           GHC.Generics
import           PackingPar.CardinalityLookup
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as AI
import Control.Parallel.Strategies
import Data.Word
import qualified Data.BitVector as BV

-- | Define the data type: 
-- 1) all data is stored in vectors
-- 2) bits can be packed adaptively (based on the part of the alphabet used for each character) or statically (based on the overall alphabet)
-- 3) All data types are words to avoid sign issues, and can be of length 16 or 64
data ParEnabledNode a = EmptyPackNode | ParNode {packMode :: ParPackMode, bits :: (V.Vector a), parMode :: ParMode}
                            deriving (Eq, Generic, Show)

data ParMode = GPU | CPU | Normal deriving (Eq, Generic, Show)

data ParPackMode = A16 | S16 | A64 | S64  deriving (Eq, Generic, Show)


instance (NFData a) => NFData (ParEnabledNode a)
--instance NFData BV.BV where
--    rnf bv = (\ !_ -> ()) bv
instance NFData ParMode
instance NFData ParPackMode

cpuCores = 8
gpuCores = 100

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

-- | Make the instance
--instance Bits ParEnabledNode where
-- | And function: throws errors for different length bits, and of any empty node returns an empty
instance (Eq a, Bits a, A.IsIntegral a, A.Elt a, Integral a) => Bits (ParEnabledNode a) where
    (.&.) node1 node2 
        | parMode node1 /= parMode node2 = error "Parallel modes do not match"
        | (parMode node1) == GPU = gpuParTwo (.&.) node1 node2
        | (parMode node1) == CPU = cpuParTwo (.&.) node1 node2
        | (parMode node1) == Normal = parTwo (.&.) node1 node2
        | otherwise = error "Unrecognized mode error"

    (.|.) node1 node2
        | parMode node1 /= parMode node2 = error "Parallel modes do not match"
        | (parMode node1) == GPU = gpuParTwo (.|.) node1 node2
        | (parMode node1) == CPU = cpuParTwo (.|.) node1 node2
        | (parMode node1) == Normal = parTwo (.|.) node1 node2
        | otherwise = error "Unrecognized mode error"

    xor node1 node2 = (.&.) ((.|.) node1 node2) (complement $ (.&.) node1 node2)

    complement node 
        | (parMode node) == GPU = gpuParOne complement node
        | (parMode node) == CPU = cpuParOne complement node
        | (parMode node) == Normal = parOne complement node
        | otherwise = error "Unrecognized mode error"

    shift node val
        | (parMode node) == GPU = gpuParOne (flip shift val) node
        | (parMode node) == CPU = cpuParOne (flip shift val) node
        | (parMode node) == Normal = parOne (flip shift val) node
        | otherwise = error "Unrecognized mode error"

    rotate node val
        | (parMode node) == GPU = gpuParOne (flip rotate val) node
        | (parMode node) == CPU = cpuParOne (flip rotate val) node
        | (parMode node) == Normal = parOne (flip rotate val) node
        | otherwise = error "Unrecognized mode error"

    bitSize EmptyPackNode = 0
    bitSize (ParNode mode bit _)
        | (mode == A16) || (mode == S16) = (V.length bit) * 16
        | (mode == A64) || (mode == S64) = (V.length bit) * 64
        -- | mode == SInf = V.foldr (\b acc -> bitSize b + acc) 0 bit
        | otherwise = error "Node mode error"

    bitSizeMaybe node = Just (bitSize node)

    testBit EmptyPackNode _ = False
    testBit (ParNode mode bit _) index
        | (mode == A16) || (mode == S16) = 
            let
                myBit = div index 16
                myPos = rem index 16
            in testBit (bit V.! myBit) myPos
        | (mode == A64) || (mode == S64) = 
            let 
                myBit = div index 64
                myPos = rem index 64
            in testBit (bit V.! myBit) myPos
        -- | mode == SInf = error "Cannot testBit on SInf"
            --let 
            --    bitSizes = V.map BV.size bit
            --    (lenSum, lens) = mapAccumR (\s acc -> (acc + s, acc + s)) 0 bitSizes
            --    bitPos = V.length $ V.takeWhile (< index) lens
            --    myPos = rem index bitPos
            --in testBit (bit V.! bitPos) myPos
        | otherwise = error "Node mode error"

    popCount EmptyPackNode = 0
    popCount node@(ParNode mode bit par)
        | (mode == A16) || (mode == S16) = V.foldr (\b acc -> acc + (cardinalityLookup b cardTable)) 0 bit
        | (mode == A64) || (mode == S64) = V.foldr (\b acc -> (popCount b) + acc) 0 bit
        -- | mode == SInf = V.foldr (\b acc -> (popCount b) + acc) 0 bit
        | otherwise = error "Node mode error"

    isSigned _ = False

    bit _ = EmptyPackNode

gpuParTwo :: (Bits b, A.IsIntegral b, A.Elt b) => (A.Exp b -> A.Exp b -> A.Exp b) -> ParEnabledNode b -> ParEnabledNode b -> ParEnabledNode b
gpuParTwo _ EmptyPackNode _ = EmptyPackNode
gpuParTwo _ _ EmptyPackNode = EmptyPackNode
gpuParTwo f (ParNode mode1 bit1 par1) (ParNode mode2 bit2 par2) 
    | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
    | mode1 /= mode2 = error "Attempt to take and of bits of different modes"
    | (par1 /= GPU) || (par2 /= GPU) = error "Attempt to GPU parallelize non-GPU nodes"
    | otherwise = 
        let 
            shape = A.Z A.:. (V.length bit1)
            array1 = A.fromList shape (V.toList bit1) 
            array2 = A.fromList shape (V.toList bit2) 
            a  = AI.run $ A.zipWith (\b1 b2 -> f b1 b2) (A.use array1) (A.use array2)
        in ParNode {packMode = mode1, bits = V.fromList $ A.toList a, parMode = par1}

cpuParTwo :: (Bits b, A.IsIntegral b, A.Elt b) => (b -> b -> b) -> ParEnabledNode b -> ParEnabledNode b -> ParEnabledNode b
cpuParTwo _ EmptyPackNode _ = EmptyPackNode
cpuParTwo _ _ EmptyPackNode = EmptyPackNode
cpuParTwo f (ParNode mode1 bit1 par1) (ParNode mode2 bit2 par2) 
    | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
    | mode1 /= mode2 = error "Attempt to take and of bits of different modes"
    | (par1 /= CPU) || (par2 /= CPU) = error "Attempt to CPU parallelize non-CPU nodes"
    | otherwise = 
        let outBit = (zipWith (\b1 b2 -> f b1 b2) (V.toList bit1) (V.toList bit2)) `using` parListChunk cpuCores rpar
        in ParNode {packMode = mode1, bits = V.fromList outBit, parMode = par1}

parTwo :: (Bits b, A.IsIntegral b, A.Elt b) => (b -> b -> b) -> ParEnabledNode b -> ParEnabledNode b -> ParEnabledNode b
parTwo _ EmptyPackNode _ = EmptyPackNode
parTwo _ _ EmptyPackNode = EmptyPackNode
parTwo f (ParNode mode1 bit1 par1) (ParNode mode2 bit2 par2) 
    | (V.length bit1) /= (V.length bit2) = error "Attempt to take and of bits of different lengths"
    | mode1 /= mode2 = error "Attempt to take and of bits of different modes"
    | (par1 /= Normal) || (par2 /= Normal) = error "Attempt to evaluate parallel node sequentially"
    | otherwise = 
        let outBit = V.zipWith (\b1 b2 -> f b1 b2) bit1 bit2
        in ParNode {packMode = mode1, bits = outBit, parMode = par1}

gpuParOne :: (Bits b, A.IsIntegral b, A.Elt b) => (A.Exp b -> A.Exp b) -> ParEnabledNode b -> ParEnabledNode b
gpuParOne _ EmptyPackNode = EmptyPackNode
gpuParOne f (ParNode mode bit par)
    | par /= GPU = error "Attempt to GPU parallelize non-GPU nodes"
    | otherwise = 
        let
            shape = A.Z A.:. (V.length bit)
            array = A.fromList shape (V.toList bit) 
            outBit = V.fromList $ A.toList $ AI.run $ A.map (\bs -> f bs) (A.use array)
        in ParNode {packMode = mode, bits = outBit, parMode = par}

cpuParOne :: (Bits b, A.IsIntegral b, A.Elt b) => (b -> b) -> ParEnabledNode b -> ParEnabledNode b
cpuParOne _ EmptyPackNode = EmptyPackNode
cpuParOne f (ParNode mode bit par)
    | par /= CPU = error "Attempt to CPU parallelize non-CPU nodes"
    | otherwise = 
        let outBit = V.fromList (map (\b -> f b) (V.toList bit) `using` parListChunk cpuCores rpar)
        in ParNode {packMode = mode, bits = outBit, parMode = par}

parOne :: (Bits b, A.IsIntegral b, A.Elt b) => (b -> b) -> ParEnabledNode b -> ParEnabledNode b
parOne _ EmptyPackNode = EmptyPackNode
parOne f (ParNode mode bit par) 
    | par /= Normal = error "Attempt to evaluate parallel node sequentially"
    | otherwise = 
        let outBit = V.map (\b -> complement b) bit
        in ParNode {packMode = mode, bits = outBit, parMode = par}

blockShiftAndFold :: (Bits b, A.IsIntegral b, A.Elt b) => String -> String -> ParEnabledNode b -> V.Vector Int -> Int -> ParEnabledNode b -> ParEnabledNode b
blockShiftAndFold _ _ EmptyPackNode _ _ _ = EmptyPackNode
blockShiftAndFold _ _ _ _ _ EmptyPackNode = EmptyPackNode 
blockShiftAndFold sideMode foldMode (ParNode pack inbits par) blocklens alphlen (ParNode pack2 initVal par2)
    | (pack == S16) || (pack == S64) = 
        let
            c   | sideMode == "L" && foldMode == "&" = V.zipWith (\b iVal -> foldr (\s acc -> (.&.) acc (shiftL b s)) iVal [1..alphlen-1]) inbits initVal
                | sideMode == "R" && foldMode == "&" = V.zipWith (\b iVal -> foldr (\s acc -> (.&.) acc (shiftR b s)) iVal [1..alphlen-1]) inbits initVal
                | sideMode == "L" && foldMode == "|" = V.zipWith (\b iVal -> foldr (\s acc -> (.|.) acc (shiftL b s)) iVal [1..alphlen-1]) inbits initVal
                | sideMode == "R" && foldMode == "|" = V.zipWith (\b iVal -> foldr (\s acc -> (.|.) acc (shiftR b s)) iVal [1..alphlen-1]) inbits initVal
                | otherwise = error "incorrect input for block shift and fold"
        in ParNode {packMode = pack, bits = c, parMode = par}
    | (pack == A16) || (pack == A64) = 
        let 
            c   | sideMode == "L" && foldMode == "&" = V.zipWith3 (\b len iVal -> foldr (\s acc -> (.&.) acc (shiftL b s)) iVal [1..len-1]) inbits blocklens initVal
                | sideMode == "R" && foldMode == "&" = V.zipWith3 (\b len iVal -> foldr (\s acc -> (.&.) acc (shiftR b s)) iVal [1..len-1]) inbits blocklens initVal
                | sideMode == "L" && foldMode == "|" = V.zipWith3 (\b len iVal -> foldr (\s acc -> (.|.) acc (shiftL b s)) iVal [1..len-1]) inbits blocklens initVal
                | sideMode == "R" && foldMode == "|" = V.zipWith3 (\b len iVal -> foldr (\s acc -> (.|.) acc (shiftR b s)) iVal [1..len-1]) inbits blocklens initVal
                | otherwise = error "incorrect input for block shift and fold"
        in ParNode {packMode = pack, bits = c, parMode = par}
    | otherwise = error "Problem with packing mode in blockShiftAndFold"

-- | Creation of an encoded node given the string and relevant information
makeNode :: (Bits b, A.IsIntegral b, A.Elt b) => b -> String -> M.Map Char String -> V.Vector(V.Vector [Char]) -> String -> V.Vector [Int] -> ParPackMode -> ParMode -> ParEnabledNode b
--makeNode hackZero string special bitAlphs overallAlph shuffles mode par | trace ("make node " ++ show string) False = undefined
makeNode hackZero string special bitAlphs overallAlph shuffles mode par
    | mode == A16 = 
        let bit = V.zipWith (\alph chars -> (makeBit hackZero 16 chars special alph overallAlph mode) + hackZero) bitAlphs remixString
        in ParNode {packMode = A16, bits = bit, parMode = par}

    | mode == S16 && (null string) = ParNode {packMode = S16, bits = V.empty, parMode = par}
    | mode == S16 =  
        let 
            numfit = div 16 (length overallAlph)
            firstbit = take numfit string
            tailbits = drop numfit string
            tailOut = makeNode hackZero tailbits special bitAlphs overallAlph shuffles mode par
            outVec = (V.singleton $ (makeBit hackZero 16 firstbit special (V.head bitAlphs) overallAlph mode) + hackZero) V.++ (bits tailOut)
        in ParNode {packMode = S16, bits = outVec, parMode = par}

    | mode == A64 = 
        let bit = V.zipWith (\alph chars -> makeBit hackZero 64 chars special alph overallAlph mode) bitAlphs remixString
        in ParNode {packMode = A64, bits = bit, parMode = par}

    | mode == S64 && (null string) =  --trace ("null string")
                                        ParNode {packMode = S64, bits = V.empty, parMode = par}
    | mode == S64 = 
        let
            numfit = div 64 (length overallAlph)
            firstbit = take numfit string
            tailbits = drop  numfit string
            tailOut = makeNode hackZero tailbits special bitAlphs overallAlph shuffles mode par
            outVec = (V.singleton $ (makeBit hackZero 64 firstbit special (V.head bitAlphs) overallAlph mode) + hackZero) V.++ (bits tailOut)
        in --trace ("outVec")
            ParNode {packMode = S64, bits = outVec, parMode = par}

    | otherwise = error "incorrect packing mode, cannot create node "
        where remixString = V.map (\indices -> (map (\i -> string !! i) indices)) shuffles

makeBit :: (Bits b, A.IsIntegral b, A.Elt b) => b -> Int -> String -> M.Map Char String -> V.Vector String -> String -> ParPackMode -> b
--makeBit hackVal len string special alphabets overallAlph mode | trace ("makeBit with alph " ++ show overallAlph) False = undefined
makeBit hackVal len string special alphabets overallAlph mode -- hackVal allows the type to be flexible
    | (mode == A16 || mode == A64) && False `V.elem` charCheck = error ("Error in makeBit, character " ++ show convstr ++ " not in the corresponding alphabet")
    | (mode == S16 || mode == S64) && False `V.elem` charCheckSingle = error ("Error in makeBit, character " ++ show convstr ++ " not in the corresponding alphabet")
    | mode == A16 = --trace "A16" $
        let 
            blocklen = length $ V.head alphabets 
            indices = V.fromList [0..(V.length convstr)-1]
            setbits = V.zipWith3 (\i str alph -> foldr (\char acc -> (.|.) acc (setBit 0 (15 - (i*blocklen) - (fromJust $ elemIndex char alph)))) hackZero str) indices recodeS alphabets 
        in (V.foldr (\b acc -> (.|.) acc b) hackZero setbits)
    | mode == S16 = --trace "S16" $
        let 
            blocklen = length overallAlph
            setbits = V.imap (\i str -> foldr (\char acc -> (.|.) acc (setBit 0 (15 - (i*blocklen) - (fromJust $ elemIndex char overallAlph)))) hackZero str) recodeS
        in (V.foldr (\b acc -> (.|.) acc b) hackZero setbits)
    | mode == A64 = --trace "A64" $
        let 
            blocklen = length $ V.head alphabets 
            indices = V.fromList [0..(V.length convstr)-1]
            setbits = V.zipWith3 (\i str alph -> foldr (\char acc -> (.|.) acc (setBit 0 (63 - (i*blocklen) - (fromJust $ elemIndex char alph)))) hackZero str) indices recodeS alphabets 
        in (V.foldr (\b acc -> (.|.) acc b) hackZero setbits)
    | mode == S64 = --trace "S64" $ 
        let 
            blocklen = length overallAlph
            setbits = V.imap (\i str -> foldr (\char acc -> (.|.) acc (setBit 0 (63 - (i*blocklen) - (fromJust $ elemIndex char overallAlph)))) hackZero str) recodeS
        in (V.foldr (\b acc -> (.|.) acc b) hackZero setbits)
    | otherwise = error "Incorrect mode in make bit"
        where
            convstr = V.fromList string
            specialKeys = M.keys special
            recodeS = --trace ("recode with convstr " ++ show convstr)
                        V.foldr (\c acc -> if c `elem` specialKeys then (special M.! c) `V.cons` acc else [c] `V.cons` acc) V.empty convstr
            charCheck = --trace ("recoded" ++ show recodeS)
                            V.zipWith (\str alph -> foldr (\char acc -> if char `elem` alph then acc else False) True str) recodeS alphabets
            charCheckSingle = --trace ("charCheck " ++ show charCheck)
                                V.map (\str -> foldr (\char acc -> if char `elem` overallAlph then acc else False) True str) recodeS
            hackZero = hackVal `xor` hackVal --ensures that hackVal is zero

occupancyMask :: (Bits b, A.IsIntegral b, A.Elt b) => b -> V.Vector Int -> V.Vector Int -> Int -> Int -> ParPackMode -> ParMode -> ParEnabledNode b
occupancyMask hackVal blockLens numCharVec alphLen numChars mode par
    | mode == A16 = 
        let b = V.zipWith (\len nc -> shift (complement hackZero) (16 - (len * nc))) blockLens numCharVec
        in ParNode {bits = b, packMode = A16, parMode = par}
    | mode == A64 = 
        let b = V.zipWith (\len nc -> shift (complement hackZero) (64 - (len * nc))) blockLens numCharVec
        in ParNode {bits = b, packMode = A64, parMode = par}
    | mode == S16 = 
        let 
            numfit = div 16 alphLen
            needBits = (ceiling $ (fromIntegral numChars :: Float) / (fromIntegral numfit :: Float)) :: Int
            leftovers = (alphLen * numChars) - (alphLen * numfit * (needBits - 1))
            bitsFull = alphLen * numfit
            fullMask = shift (complement hackZero) (16 - bitsFull)
            leftoverMask = --trace ("leftovers "++ show leftovers) 
                            shift (complement hackZero) (16 - leftovers)
            b = V.replicate (needBits - 1) fullMask V.++ V.singleton leftoverMask
        in ParNode {bits = b, packMode = S16, parMode = par}
    | mode == S64 =
        let 
            numfit = div 64 alphLen
            needBits = (ceiling $ (fromIntegral numChars :: Float) / (fromIntegral numfit :: Float)) :: Int
            leftovers = (alphLen * numChars) - (alphLen * numfit * (needBits - 1))
            bitsFull = alphLen * numfit
            fullMask = shift (complement hackZero) (64 - bitsFull)
            leftoverMask = shift (complement hackZero) (64 - leftovers)
            b = V.replicate (needBits - 1) fullMask V.++ V.singleton leftoverMask 
        in ParNode {bits = b, packMode = S64, parMode = par}
    | otherwise = error "incorrect mode for for occupancy mask"
        where hackZero = hackVal `xor` hackVal

standardMask :: (Bits b, A.IsIntegral b, A.Elt b) => V.Vector Int -> ParEnabledNode b -> Int -> ParEnabledNode b
standardMask _ EmptyPackNode _ = EmptyPackNode
standardMask blockLens occMask@(ParNode mode mask par) alphLen
    | mode == A16 && V.null blockLens = ParNode {packMode = mode, bits = V.empty, parMode = par}
    | mode == A16 = 
        let         
            b = V.head blockLens
            positions = [x | x <- [0..15], (16 - x) `mod` b == 0]
            standard = foldr (\i acc -> 2^i + acc) hackZero positions
            maskStandard = (.&.) standard (V.head mask)
            restMask = standardMask (V.tail blockLens) (ParNode {packMode = mode, bits = V.tail mask, parMode = par}) alphLen 
        in ParNode {packMode = mode, bits = maskStandard `V.cons` (bits restMask), parMode = par}
    | mode == A64  && V.null blockLens = ParNode {packMode = mode, bits = V.empty, parMode = par}
    | mode == A64 = 
        let         
            b = V.head blockLens
            positions = [x | x <- [0..63], (64 - x) `mod` b == 0]
            standard = foldr (\i acc -> 2^i + acc) hackZero positions
            maskStandard = (.&.) standard (V.head mask)
            restMask = standardMask (V.tail blockLens) (ParNode {packMode = mode, bits = V.tail mask, parMode = par}) alphLen 
        in ParNode {packMode = mode, bits = maskStandard `V.cons` (bits restMask), parMode = par}
    | mode == S16 = 
        let 
            positions = [x | x <- [0..15], (16 - x) `mod` alphLen == 0]
            standard = foldr (\i acc -> 2^i + acc) hackZero positions
            b = V.map (\m -> (.&.) standard m) mask
        in ParNode {packMode = mode, bits = b, parMode = par}
    | mode == S64 =
        let 
            positions = [x | x <- [0..63], (64 - x) `mod` alphLen == 0]
            standard = foldr (\i acc -> 2^i + acc) hackZero positions
            b = V.map (\m -> (.&.) standard m) mask
        in ParNode {packMode = mode, bits = b, parMode = par}
    | otherwise = error "incorrect mode for for occupancy mask"
        where hackZero = ((bits occMask) V.! 0) `xor` ((bits occMask) V.! 0)

-- | Unified function to get both the occupancy and standard masks
genMasks :: (Bits b, A.IsIntegral b, A.Elt b) => b -> V.Vector Int -> V.Vector Int -> Int -> Int -> ParPackMode -> ParMode -> (ParEnabledNode b, ParEnabledNode b)
genMasks hackVal blockLens numCharVec alphLen numChars mode par = 
    let
        occMask = occupancyMask hackVal blockLens numCharVec alphLen numChars mode par
        sMask = standardMask blockLens occMask alphLen
    in (occMask, sMask)

-- | Get the cost, intelligently figuring out whether it's adaptive or non-adaptive
-- this function allows for the cost to be divided by the alphabet length at each position
getNodeCost :: (Bits b, A.IsIntegral b, A.Elt b, Integral b) => ParEnabledNode b -> V.Vector Int -> Int -> Float
--getNodeCost node mode blockLens alphLen | trace ("cost of node " ++ show node) False = undefined
getNodeCost EmptyPackNode _ _ = 0
getNodeCost node@(ParNode mode bit par) blockLens alphLen
    | mode == S16 || mode == S64 = fromIntegral $ div (popCount node) alphLen  
    | mode == A16 = 
        let 
            counts = V.map (\b -> cardinalityLookup b cardTable) bit
            costs = V.zipWith (\l c -> div c l) blockLens counts
        in fromIntegral $ V.sum costs
    | otherwise =         
        let 
            counts = V.map (\b -> popCount b) bit
            costs = V.zipWith (\l c -> div c l) blockLens counts
        in fromIntegral $ V.sum costs

