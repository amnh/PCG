-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Metadata.Parsed
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Typeclass for metadata extracted from parsed results
--
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module Bio.Metadata.Parsed where

import           Bio.Metadata.Internal
import           Bio.Sequence.Parsed
import           Bio.PhyloGraph.Solution
import           Data.Char
import           Data.Foldable
import           Data.List
import qualified Data.Map.Lazy          as M
import           Data.Maybe
import           Data.Monoid
import           Data.Vector                   (fromList, Vector)
import qualified Data.Vector            as V
import           File.Format.Fasta             (FastaParseResult,TaxonSequenceMap)
import           File.Format.Fastc
import           File.Format.Newick
import           File.Format.Nexus      hiding (CharacterMetadata, DNA, RNA, Nucleotide, TaxonSequenceMap)
import qualified File.Format.Nexus.Data as Nex
import qualified File.Format.TNT        as TNT
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot

-- | Represents a parser result type which can have a character metadata
--   structure extracted from it.
class ParsedMetadata a where
    unifyMetadata :: a -> Vector StandardMetadata

instance ParsedMetadata FastaParseResult where
    unifyMetadata = makeEncodeInfo . unifyCharacters

instance ParsedMetadata TaxonSequenceMap where
    unifyMetadata = makeEncodeInfo . unifyCharacters

instance ParsedMetadata FastcParseResult where
    unifyMetadata = makeEncodeInfo . unifyCharacters

instance ParsedMetadata NewickForest where
    unifyMetadata _ = mempty

instance ParsedMetadata TNT.TntResult where
    unifyMetadata (Left _) = mempty
    unifyMetadata (Right withSeq) = fromList $ zipWith f (toList $ TNT.charMetaData withSeq) (snd . head . toList $ TNT.sequences withSeq)
        where
           f :: Monoid s => TNT.CharacterMetaData -> TNT.TntCharacter -> (CharacterMetadata s)
           f inMeta inChar =  let defaultMeta = makeOneInfo $ tntAlphabet inChar
                    in  defaultMeta { name       = TNT.characterName   inMeta
                                    , stateNames = TNT.characterStates inMeta
                                    , tcm        = fromMaybe (tcm defaultMeta) (TNT.costTCM inMeta)
                                    }
           tntAlphabet TNT.Continuous {} = mempty
           tntAlphabet TNT.Discrete   {} = disAlph -- TODO: get subset of maximum alphabet by doing a columwise set collection
           tntAlphabet TNT.Dna        {} = dnaAlph
           tntAlphabet TNT.Protein    {} = aaAlph

instance ParsedMetadata TCM where
    unifyMetadata (TCM alph mat) = 
        let defaultMeta = makeOneInfo (toList alph)
        in  pure (defaultMeta {tcm = mat})

instance ParsedMetadata VertexEdgeRoot where
    unifyMetadata _ = mempty

instance ParsedMetadata Nexus where
    unifyMetadata (Nexus (_, metas)) = V.map convertNexusMeta metas
        where
            convertNexusMeta inMeta = 
                let defaultMeta = makeOneInfo (Nex.alphabet inMeta)
                in  defaultMeta { name = Nex.name inMeta, isIgnored = Nex.ignored inMeta, 
                                  tcm  = fromMaybe (tcm defaultMeta) (transitionCosts <$> Nex.costM inMeta)}

disAlph, dnaAlph, rnaAlph, aaAlph :: [String]
-- | The acceptable DNA character values (with IUPAC codes).
dnaAlph = pure <$> addOtherCases "AGCTRMWSKTVDHBNX?-"
-- | The acceptable RNA character values (with IUPAC codes).
rnaAlph = pure <$> addOtherCases "AGCURMWSKTVDHBNX?-"
-- | The acceptable amino acid/protein character values (with IUPAC codes).
aaAlph  = pure <$> addOtherCases "ABCDEFGHIKLMNPQRSTVWXYZ-"
-- | The acceptable discrete character values.
disAlph = pure <$> (['0'..'9'] <> ['A'..'Z'] <> ['a'..'z'] <> "-" <> "?")

-- | Adds case insensitive values to a 'String'.
addOtherCases :: String -> String
addOtherCases [] = []
addOtherCases (x:xs)
  | isLower x && toUpper x `notElem` xs = (toUpper x) : x : addOtherCases xs
  | isUpper x && toLower x `notElem` xs = x : (toLower x) : addOtherCases xs
  | otherwise = x : addOtherCases xs

-- | Useful function to check subsets of lists
subsetOf :: (Ord a) => [a] -> [a] -> Bool
subsetOf list1 list2 = foldr (\e acc -> acc && e `elem` list2) True list1

-- | Make a single info given an alphabet
makeOneInfo :: Monoid s => Alphabet -> CharacterMetadata s
makeOneInfo alph = CharMeta DirectOptimization alph mempty False False 1 mempty mempty (mempty, mempty) 1
{-
     | alph `subsetOf` dnaAlph || alph `subsetOf` rnaAlph = def {charType = Nucleotide}
     | alph `subsetOf` aaAlph = def {charType = AminoAcid}
     | otherwise = def {charType = Custom}
     where def = CharMeta Unknown alph mempty False False 1 mempty mempty (mempty, mempty) 1
-}

-- | Functionality to make char info from tree seqs
makeEncodeInfo :: Monoid s => TreeSeqs -> Vector (CharacterMetadata s)
makeEncodeInfo seqs = V.map makeOneInfo alphabets
    where alphabets = developAlphabets seqs

-- | Internal function to create alphabets
developAlphabets :: TreeSeqs -> Vector Alphabet
developAlphabets inSeqs = V.map setGapChar $ V.map sort $ M.foldr (V.zipWith getNodeAlphAt) initializer inSeqs
    where
        someSeq = head $ M.elems inSeqs
        initializer = if null someSeq then mempty
                        else V.replicate (V.length someSeq) mempty

        getNodeAlphAt :: Maybe ParsedSeq -> Alphabet -> Alphabet
        --getNodeAlphAt inSeq soFar | trace ("getNodeAlphAt " ++ show inSeq ++ " with accum " ++ show soFar) False = undefined
        getNodeAlphAt inSeq soFar
            | isNothing inSeq = mempty
            | otherwise =  V.foldr (flip $ foldr (\sIn prev -> if sIn `elem` prev then prev else sIn : prev)) soFar (fromJust inSeq)

-- | Ensure that the gap char is present and correctly positioned in an alphabet
setGapChar :: Alphabet -> Alphabet
--setGapChar inAlph | trace ("setGapChar " ++ show inAlph) False = undefined
setGapChar inAlph = filter (/= "-") inAlph ++ ["-"]
