module File.Format.Conversion.Encoder where

import           Bio.Phylogeny.Graph
import           Bio.Phylogeny.PhyloCharacter
import           Bio.Sequence.Coded
import           Bio.Sequence.Parsed
import           Data.BitVector        hiding (foldr, replicate)
import           Data.Int
import           Data.List             hiding (replicate, zipWith)
import qualified Data.Map.Lazy         as M
import           Data.Matrix.NotStupid              (matrix)
import           Data.Vector           hiding ((++), length, head, elem, foldr, map)
import qualified Data.Vector           as V   (foldr)
import           Prelude               hiding (replicate, zipWith)

dnaAlph, rnaAlph :: [String]
dnaAlph = ["A", "C", "G", "T", "-"] 
rnaAlph = ["A", "C", "G", "U", "-"]

makeEncodeInfo :: TreeSeqs -> Vector CharInfo
makeEncodeInfo seqs = makeOneInfo alphabets firstSeq
    where
        alphabets = developAlphabets seqs
        firstSeq = head $ M.elems seqs

developAlphabets :: TreeSeqs -> Alphabet
developAlphabets seqs = V.foldr (zipWith develop) (replicate numChars []) (fromList $ M.elems seqs)
    where
        numChars = length . head $ M.elems seqs
        develop :: Alphabet -> [String] -> [String]
        develop inSeq inAlph = 
            let alph = foldr (flip foldr (\c acc -> if c `elem` acc then acc else c : acc)) [] inSeq
            in nub (alph ++ inAlph)

makeOneInfo :: Alphabet -> ParsedSeq -> Vector (PhyloCharacter Int64)
makeOneInfo = zipWith charInfo
    where
        charInfo :: [String] -> Vector [String] -> PhyloCharacter Int64
        charInfo alph _ 
            | alph `subsetOf` dnaAlph = DNA False (empty, empty) (fromList alph) defaultMat
            | alph `subsetOf` rnaAlph = RNA False (empty, empty) (fromList alph) defaultMat
            | otherwise = Custom False (empty, empty) (fromList alph) defaultMat
                where defaultMat = matrix (length alph) (length alph) (const 1)

subsetOf :: (Ord a) => [a] -> [a] -> Bool
subsetOf list1 list2 = foldr (\e acc -> acc && e `elem` list2) True list1

encodeIt :: ParsedSequences -> Vector CharInfo -> EncodedSequences BitVector
encodeIt inSeqs inInfos = zipWith encodeOne inSeqs inInfos
    where
        encodeOne :: ParsedSeq -> CharInfo -> EncodedSeq BitVector
        encodeOne inSeq info = Just . singleton $ fromBits tfList
            where
                tfList = foldr (\pos acc -> map (`elem` pos) alph ++ acc) [] inSeq
                alph   = toList $ alphabet info 

packIt :: ParsedSequences -> Vector CharInfo -> EncodedSequences BitVector
packIt = encodeIt

