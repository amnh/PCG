-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Report.ImpliedalignmentFasta
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard algorithm for implied alignment
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module PCG.Command.Types.Report.ImpliedAlignmentFasta where

import Analysis.ImpliedAlignment.Internal
import Bio.Character.Dynamic.Coded
import Bio.Metadata   hiding (name)
import Bio.PhyloGraph.DAG
import Bio.PhyloGraph.Network (nodeIsLeaf)
import Bio.PhyloGraph.Node
import Bio.PhyloGraph.Solution
--import Control.DeepSeq       (force)
import Data.Alphabet
import Data.Foldable
import Data.IntMap           (IntMap,insert,keys)
import Data.Key
import Data.List             (intercalate)
import Data.List.Utility     (chunksOf)
import Data.Map              (Map, singleton)
import Data.Monoid           ((<>))
import Data.Vector           (Vector)
-- TODO: Remove this import when Data.Vector.Instances gets fixed
import qualified Data.Vector as V ((!))
import Data.Vector.Instances ()
import Prelude        hiding (lookup,zipWith)

import Debug.Trace (trace)

--iaOutput :: (MetadataSolution s m, GeneralSolution s f) => AlignmentSolution DynamicChar -> s -> [(FilePath, String)]
iaOutput' :: StandardSolution -> [(FilePath, String)]
--iaOutput align solution | trace (mconcat [show align, show solution]) False = undefined
iaOutput' solution = {- (\x -> trace (intercalate "\n\n"
                                         [ integrityCheckSolution solution
                                         , renderAlignments align
                                         , "DynamicChar indicies: "     <> show (keys dynamicCharacterIndicesAndAlphabets)
                                         , "Metadata character types: " <> show (getType <$> getMetadata solution)
                                         ]
                                       ) x) $
                      -}
                         foldMapWithKey characterToFastaFile dynamicCharacterIndicesAndAlphabets 
  where
    -- Here we use the metadata to filter for dynamic character indicies and
    -- their corresponding alphabets. 
    dynamicCharacterIndicesAndAlphabets :: IntMap (Alphabet String)
    dynamicCharacterIndicesAndAlphabets = foldlWithKey dynamicCharFilter mempty (getMetadata solution)
      where
        dynamicCharFilter im i e = if   getType e == DirectOptimization
                                   then insert i (getAlphabet e) im
                                   else im

    -- Here we perform a recursive structure "zip" between graph of forests of
    -- DAGs of nodes containing implied alignment sequences and the original
    -- graph of forests of DAGs of nodes containing the unaligned sequences.
    -- The result of the structure zip is a Map from the taxon name within the
    -- original (right-hand) structure to the implied alignment sequences within
    -- the new (left-hand) structure.
    nodeCharacterMapping :: Map String (Vector DynamicChar)
    nodeCharacterMapping = foldMap f $ getForests solution
      where
        f solutionForest = foldMap g solutionForest
        g dag            = foldMap h $getNodes dag
          where
            h node       = if   node `nodeIsLeaf` dag
                           then singleton (name node) (getHomologies' node)
                           else mempty

    -- The folding function for consuming the 'dynamicCharacterIndicesAndAlphabets'
    -- structure above. For each character index and corresponding alphabet this
    -- function will create the file name and the file contents in FASTA format.
    -- Internally folds over the 'nodeCharacterMapping' structure to place all
    -- taxa in the graph into the resulting file.
    --
    -- Because 'dynamicCharacterIndicesAndAlphabets' contains only dynamic
    -- characters, the resulting list of file content will contain a exactly one
    -- file content tuple for each dynamic character.
    --
    -- Type checker doesn't like the Int in this explicit type signature.
    -- The type checker *can* infer the correct (complicated) type all by itself,
    -- so we will let it do that rather than listen to it complain.
    
    -- characterToFastaFile :: Int -> Alphabet -> [(FilePath, String)]
    characterToFastaFile i alpha = [(characterFileName, foldMapWithKey f nodeCharacterMapping)]
      where
        characterFileName = mconcat ["Character", show i, ".fasta"]
        f nodeName characters =
          case i `lookup` characters of
            Nothing        -> titleLine <> "\n (No sequence)\n"
            Just character -> dynamicCharacterToFastaBlock character
          where
            titleLine     = "> " <> nodeName
            dynamicCharacterToFastaBlock character = unlines $ titleLine : sequenceLines <> [""]
              where
                sequenceLines = chunksOf 50 . concatMap renderAmbiguityGroup . toList $ decodeDynamic alpha character
                renderAmbiguityGroup [x] = x
                renderAmbiguityGroup xs  = "[" <> concat xs <> "]"

--iaOutput :: (MetadataSolution s m, GeneralSolution s f) => AlignmentSolution DynamicChar -> s -> [(FilePath, String)]
iaOutput :: AlignmentSolution DynamicChar -> StandardSolution -> [(FilePath, String)]
--iaOutput align solution | trace (mconcat [show align, show solution]) False = undefined
iaOutput align solution = (\x -> trace (intercalate "\n\n"
                                         [ integrityCheckSolution solution
                                         , renderAlignments align
                                         , "DynamicChar indicies: "     <> show (keys dynamicCharacterIndicesAndAlphabets)
                                         , "Metadata character types: " <> show (getType <$> getMetadata solution)
                                         ]
                                       ) x) $
                         foldMapWithKey characterToFastaFile dynamicCharacterIndicesAndAlphabets 
  where
    -- Here we use the metadata to filter for dynamic character indicies and
    -- their corresponding alphabets. 
    dynamicCharacterIndicesAndAlphabets :: IntMap (Alphabet String)
    dynamicCharacterIndicesAndAlphabets = foldlWithKey dynamicCharFilter mempty (getMetadata solution)
      where
        dynamicCharFilter im i e = if   getType e == DirectOptimization
                                   then insert i (getAlphabet e) im
                                   else im

    -- Here we perform a recursive structure "zip" between graph of forests of
    -- DAGs of nodes containing implied alignment sequences and the original
    -- graph of forests of DAGs of nodes containing the unaligned sequences.
    -- The result of the structure zip is a Map from the taxon name within the
    -- original (right-hand) structure to the implied alignment sequences within
    -- the new (left-hand) structure.
    nodeCharacterMapping :: Map String (Vector DynamicChar)
    nodeCharacterMapping = mconcat $ zipWith f align (getForests solution)
      where
        f alignedForest solutionForest = mconcat $ zipWith g alignedForest solutionForest
        g alignment     dag            = foldMapWithKey h alignment
          where
            nodeRefs = getNodes dag
            h :: Int -> Vector DynamicChar -> Map String (Vector DynamicChar)
            h i characters = singleton (name $ nodeRefs V.! i) characters

    -- The folding function for consuming the 'dynamicCharacterIndicesAndAlphabets'
    -- structure above. For each character index and corresponding alphabet this
    -- function will create the file name and the file contents in FASTA format.
    -- Internally folds over the 'nodeCharacterMapping' structure to place all
    -- taxa in the graph into the resulting file.
    --
    -- Because 'dynamicCharacterIndicesAndAlphabets' contains only dynamic
    -- characters, the resulting list of file content will contain a exactly one
    -- file content tuple for each dynamic character.
    --
    -- Type checker doesn't like the Int in this explicit type signature.
    -- The type checker *can* infer the correct (complicated) type all by itself,
    -- so we will let it do that rather than listen to it complain.
    
    -- characterToFastaFile :: Int -> Alphabet -> [(FilePath, String)]
    characterToFastaFile i alpha = [(characterFileName, foldMapWithKey f nodeCharacterMapping)]
      where
        characterFileName = mconcat ["Character", show i, ".fasta"]
        f nodeName characters =
          case i `lookup` characters of
            Nothing        -> titleLine <> "\n (No sequence)\n"
            Just character -> dynamicCharacterToFastaBlock character
          where
            titleLine     = "> " <> nodeName
            dynamicCharacterToFastaBlock character = unlines $ titleLine : sequenceLines <> [""]
              where
                sequenceLines = chunksOf 50 . concatMap renderAmbiguityGroup . toList $ decodeDynamic alpha character
                renderAmbiguityGroup [x] = x
                renderAmbiguityGroup xs  = "[" <> concat xs <> "]"

integrityCheckSolution :: StandardSolution -> String
integrityCheckSolution sol = ("Solution:\n" <>) . unlines' $ f <#$> getForests sol
      where
        f i forest = mconcat ["Forest ", show i, ": \n", unlines' $ g <#$> forest]
          where
            g :: Show a => a -> DAG -> String
            g j dag = prefix
                    $ if   failures == 0
                      then "OK"
                      else mconcat ["Failures ", show failures, " ", wrap $ intercalate ","  raw]
              where
                prefix x = " * DAG " <> show j <> " integrity check: " <> x
                (failures, raw) = foldrWithKey h accum $ nodes dag 
                accum = (0, []) :: (Int, [String])
                h k e (incorrect, xs) = ( if   k /= nodeIdx e
                                          then incorrect + 1
                                          else incorrect
                                        , show (nodeIdx e) : xs
                                        )
                wrap x = "[" <> x <> "]"

renderAlignments :: AlignmentSolution DynamicChar -> String
renderAlignments alignments = ("Alignments:\n" <>) . unlines' $ f <#$> alignments
      where
        f i forest = mconcat ["Forest ", show i, ": \n", unlines' $ g <#$> forest]
          where
            g :: Show a => a -> IntMap (Vector DynamicChar) -> String
            g j intmap = prefix <> suffix
              where
                prefix = " * IntMap " <> show j <> " : "
                suffix = wrap . intercalate "," $ foldMapWithKey h intmap
                h k e  = pure $ mconcat [show k, "{", show $ length e, "}"]
                wrap x = "[" <> x <> "]" 

unlines' :: [String] -> String
unlines' = intercalate "\n"
