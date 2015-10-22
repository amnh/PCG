{-# LANGUAGE BangPatterns, DeriveGeneric #-}
module PackingPar.ParEnabledNode (ParEnabledNode (..), 
                        (B..&.)) where

-- | imports
import           Control.DeepSeq
import qualified Data.Bits as B
import           Data.Either
import           Data.List
import           Data.Maybe
import qualified Data.Vector    as V
import           Data.Word
import qualified Data.Map       as M
import           Debug.Trace
import           GHC.Generics
import           Packing.CardinalityLookup
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as AI

-- | Define the data type: 
-- 1) all data is stored in vectors
-- 2) bits can be packed adaptively (based on the part of the alphabet used for each character) or statically (based on the overall alphabet)
-- 3) All data types are words to avoid sign issues, and can be of length 16 or 64
data ParEnabledNode = EmptyPackNode | 
						A16 {bitsA16 :: (V.Vector Word16), parMode :: ParMode} | 
						S16 {bitsS16 :: (V.Vector Word16), parMode :: ParMode} | 
						A64 {bitsA64 :: (V.Vector Word64), parMode :: ParMode} | 
						S64 {bitsS64 :: (V.Vector Word64), parMode :: ParMode} 
						--SInf {bitsSInf :: BV.BitVector, parMode :: ParMode}
						deriving (Eq, Generic, Show)

data ParMode = GPU | CPU | Normal deriving (Eq, Generic, Show)

instance NFData ParEnabledNode
--instance NFData BV.BV where
--    rnf bv = (\ !_ -> ()) bv
instance NFData ParMode

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
instance B.Bits ParEnabledNode where
	(.&.) node1 node2 
		| parMode node1 /= parMode node2 = error "Parallel modes do not match"
		| (parMode node1) == GPU = gpuParTwo16 (B..&.) node1 node2
		-- | (parMode node1) == CPU = cpuParTwo (B..&.) node1 node2
		-- | (parMode node1) == Normal = parTwo (B..&.) node1 node2
		| otherwise = error "Unrecognized mode error"

gpuParTwo16 :: (A.IsIntegral a, B.Bits a, A.IsNum a, A.IsScalar a, A.Elt a) => (A.Exp a-> A.Exp a-> A.Exp a) -> ParEnabledNode -> ParEnabledNode -> ParEnabledNode
gpuParTwo16 f EmptyPackNode _ = EmptyPackNode
gpuParTwo16 f _ EmptyPackNode = EmptyPackNode
--gpuParTwo f (SInf _ _) (SInf _ _) = error "Cannot GPU parallelize over a built-in bitvector"
gpuParTwo16 f (A16 inBits1 _) (A16 inBits2 _) = 
	let 
        shape = A.Z A.:. (V.length inBits1)
        array1 = A.fromList shape (V.toList inBits1) -- :: A.Vector A.Word16 
        array2 = A.fromList shape (V.toList inBits2) -- :: A.Vector A.Word16
        b     | (V.length inBits1) /= (V.length inBits2) = error "Attempt to take and of bits of different lengths"
              | otherwise = AI.run $ A.zipWith (f) (A.use array1) (A.use array2)
    in A16 {bitsA16 = V.fromList $ A.toList b}
gpuParTwo16 f (S16 inBits1 _) (S16 inBits2 _) = 
	let 
        shape = A.Z A.:. (V.length inBits1)
        array1 = A.fromList shape (V.toList inBits1) :: A.Vector A.Word16 
        array2 = A.fromList shape (V.toList inBits2) :: A.Vector A.Word16
        b     | (V.length inBits1) /= (V.length inBits2) = error "Attempt to take and of bits of different lengths"
              | otherwise = AI.run $ A.zipWith (\b1 b2 ->f b1 b2) (A.use array1) (A.use array2)
    in S16 {bitsS16 = V.fromList $ A.toList b}

gpuParTwo64 :: (A.Exp Word64 -> A.Exp Word64 -> A.Exp Word64) -> ParEnabledNode -> ParEnabledNode -> ParEnabledNode
gpuParTwo64 f EmptyPackNode _ = EmptyPackNode
gpuParTwo64 f _ EmptyPackNode = EmptyPackNode
gpuParTwo64 f (A64 inBits1 _) (A64 inBits2 _) = 
	let 
        shape = A.Z A.:. (V.length inBits1)
        array1 = A.fromList shape (V.toList inBits1) :: A.Vector A.Word64
        array2 = A.fromList shape (V.toList inBits2) :: A.Vector A.Word64
        b     | (V.length inBits1) /= (V.length inBits2) = error "Attempt to take and of bits of different lengths"
              | otherwise = AI.run $ A.zipWith (\b1 b2 ->f b1 b2) (A.use array1) (A.use array2)
    in A64 {bitsA64 = V.fromList $ A.toList b}
gpuParTwo64 f (S64 inBits1 _) (S64 inBits2 _) = 
	let 
        shape = A.Z A.:. (V.length inBits1)
        array1 = A.fromList shape (V.toList inBits1) :: A.Vector A.Word64 
        array2 = A.fromList shape (V.toList inBits2) :: A.Vector A.Word64
        b     | (V.length inBits1) /= (V.length inBits2) = error "Attempt to take and of bits of different lengths"
              | otherwise = AI.run $ A.zipWith (\b1 b2 ->f b1 b2) (A.use array1) (A.use array2)
    in S64 {bitsS64 = V.fromList $ A.toList b}

cpuParTwo :: (a -> a -> a) -> ParEnabledNode -> ParEnabledNode -> ParEnabledNode
cpuParTwo _ _ _ = undefined

parTwo :: (a -> a -> a) -> ParEnabledNode -> ParEnabledNode -> ParEnabledNode
parTwo _ _ _ = undefined

