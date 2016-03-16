-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Sequence.Parsed.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Typeclas for a parsed sequence
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Bio.Sequence.Parsed.Class where

import           Bio.Sequence.Parsed
import           Data.Bifunctor   (second)
import           Data.Foldable
import           Data.Key
import           Data.Map         (insert)
import qualified Data.Map    as M (fromList)
import           Data.Maybe
import           Data.Monoid
import           Data.Tree
import qualified Data.Vector as V
import           File.Format.Fasta
import           File.Format.Fastc
import           File.Format.Newick
import           File.Format.Nexus
import           File.Format.TNT
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot

-- TODO: Make sure that pipelines don't undo and redo the conversion to treeSeqs
-- currently we pack and unpack codes, make parsers dumber in the future. Read below!

-- | Instances provide a method to extract Character sequences from raw parsed results.
--   The 'TreeSeqs' are agnostic of character data types. Characters of types DNA, RNA,
--   protein, and amino acid will *not* have thier IUPAc codes translated to ambiguity
--   the apropriate groups. This abiguity group translation will occur later during the
--   rectification with the character metadata. Parsers who produce expanded ambiguity
--   groups for these character types will be collapsed back to the IUPAC code for the
--   ambiguity group.
--
--   It is expected that parsers will altered to return simpler character literals for
--   time efficientcy in the future.
class ParsedCharacters a where
    unifyCharacters :: a -> [TreeSeqs]

instance ParsedCharacters FastaParseResult where
    unifyCharacters = pure . foldr f mempty
        where
            convertSeq = V.fromList . map (Just . pure . pure . pure)
            f (FastaSequence n s) = insert n (convertSeq s)

instance ParsedCharacters FastcParseResult where
    unifyCharacters = pure . foldl f mempty
        where
            f m (FastcSequence label symbols) = insert label (pure $ pure symbols) m

instance ParsedCharacters NewickForest where
    unifyCharacters = map f 
        where
            f :: NewickNode -> TreeSeqs
            f node 
              | null (descendants node) = insert name mempty mempty
              | otherwise = foldl1 (<>) $ f <$> descendants node
              where
                  name = fromMaybe "" $ newickLabel node

instance ParsedCharacters Nexus where
    unifyCharacters _ = mempty

instance ParsedCharacters TntResult where
    unifyCharacters (Left trees) = foldl f mempty trees
      where
          f xs tree = foldl g mempty tree : xs
          g m (Index  i) = insert (show i) mempty m
          g m (Name   n) = insert n mempty m
          g m (Prefix p) = insert p mempty m
    unifyCharacters (Right (WithTaxa seqs _ []   )) = pure . M.fromList . toList $ second tntToTheSuperSequence   <$> seqs
    unifyCharacters (Right (WithTaxa seqs _ trees)) = (M.fromList . toList . fmap (second tntToTheSuperSequence)) <$> trees

tntToTheSuperSequence :: TaxonSequence -> ParsedSequences
tntToTheSuperSequence inSeq = V.fromList $ (Just . pure . f . show) <$> inSeq
  where
    f ('[':xs) = pure <$> init xs
    f e        = pure e

instance ParsedCharacters TCM where
    unifyCharacters _ = mempty

instance ParsedCharacters VertexEdgeRoot where
    unifyCharacters (VER v e r) = f . buildTree <$> toList r
        where
            es = toList e
            f node 
              | null (subForest node) = insert (rootLabel node) mempty mempty
              | otherwise = foldl1 (<>) $ f <$> subForest node
            buildTree name = Node name kids
                where
                    kids = fmap (buildTree . snd) . filter ((==name) . fst) $ edgeConnection <$> es

