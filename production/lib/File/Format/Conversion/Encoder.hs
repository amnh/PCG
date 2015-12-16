module File.Format.Conversion.Encoder where

import Prelude hiding (replicate, zipWith)
import Bio.Phylogeny.Graph
import Data.Vector hiding ((++), length, head, elem, foldr, map)
import qualified Data.Vector as V (foldr)
import Data.BitVector hiding (foldr, replicate)
import qualified Data.Map.Lazy as M
import Data.List hiding (replicate, zipWith)
import Bio.Phylogeny.PhyloCharacter
import Data.Int
import Data.Matrix (matrix)

dnaAlph, rnaAlph :: [String]
dnaAlph = ["A", "C", "G", "T", "-"] 
rnaAlph = ["A", "C", "G", "U", "-"]

type ParsedSequence = M.Map String (Vector (Vector [String]))

makeEncodeInfo :: ParsedSequence -> Vector CharInfo
makeEncodeInfo seqs = makeOneInfo alphabets firstSeq
    where
        alphabets = developAlphabets seqs
        firstSeq = head $ M.elems seqs

developAlphabets :: ParsedSequence -> Vector [String]
developAlphabets seqs = V.foldr (\node acc -> zipWith develop node acc) (replicate numChars []) (fromList $ M.elems seqs)

    where
        numChars = length $ head $ M.elems seqs

        develop :: Vector [String] -> [String] -> [String]
        develop inSeq inAlph = 
            let alph = foldr (\char acc -> foldr (\c acc2 -> if c `elem` acc2 then acc2 else c : acc2) acc char) [] inSeq
            in nub (alph ++ inAlph)

makeOneInfo :: Vector [String] -> Vector (Vector [String]) -> Vector (PhyloCharacter Int64)
makeOneInfo alphabets mySeq = zipWith charInfo alphabets mySeq
    where
        charInfo :: [String] -> Vector [String] -> PhyloCharacter Int64
        charInfo alph _ 
            | alph `subsetOf` dnaAlph = DNA False (empty, empty) (fromList alph) defaultMat
            | alph `subsetOf` rnaAlph = RNA False (empty, empty) (fromList alph) defaultMat
            | otherwise = Custom False (empty, empty) (fromList alph) defaultMat
                where defaultMat = matrix (length alph) (length alph) (const 1)

subsetOf :: (Ord a) => [a] -> [a] -> Bool
subsetOf list1 list2 = foldr (\l acc -> if l `elem` list2 then acc else False) True list1

encodeIt :: Vector (Vector [String]) -> Vector CharInfo -> Maybe (Vector (Vector BitVector))
encodeIt inSeqs inInfos = Just $ zipWith encodeOne inSeqs inInfos
    where
        encodeOne :: Vector [String] -> CharInfo -> Vector BitVector
        encodeOne inSeq info = 
            let tfList = foldr (\pos acc -> map (\a -> if a `elem` pos then True else False) alph ++ acc) [] inSeq
            in singleton $ fromBits tfList
            where
                alph = toList $ alphabet info 

packIt :: Vector (Vector [String]) -> Vector CharInfo -> Maybe (Vector (Vector BitVector))
packIt = encodeIt

