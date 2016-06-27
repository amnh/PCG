-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Parsed.Class
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

module Bio.Character.Parsed.Class where

import           Bio.Character.Parsed.Internal
import           Data.Bifunctor             (second)
import           Data.Foldable
import           Data.Map                   (Map,insert,mergeWithKey)
import qualified Data.Map          as M     (fromList)
import           Data.Maybe
import           Data.Monoid
import           Data.Tree
import qualified Data.Vector       as V
import           File.Format.Fasta
import           File.Format.Fastc
import           File.Format.Newick
import           File.Format.Nexus hiding (TaxonSequenceMap)
import           File.Format.TNT
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot

-- TODO: Make sure that pipelines don't undo and redo the conversion to treeSeqs
-- currently we pack and unpack codes, make parsers dumber in the future. Read below!

-- | Instances provide a method to extract Character sequences from raw parsed results.
--   The 'TreeSeqs' are agnostic of character data types. "Tree-only" return values from
--   files will extract the taxa labels from leaf nodes only with empty sequences.
--
--   Characters of types DNA, RNA, protein, and amino acid will *not* have thier IUPAC
--   codes translated to the apropriate groups. This abiguity group translation will
--   occur later during the rectification process with the character metadata. Parsers
--   which produce expanded ambiguity groups for these character types will be collapsed
--   back to the IUPAC code for the ambiguity group during the type-class's extraction
--   process.
--
--   It is expected that parsers will altered to return simpler character literals for
--   time efficientcy in the future.
--
-- I need to think about how this might interact with some things in Nexus, but it seems
-- to make sense. It might make verification in the parsers more difficult... thinking...

class ParsedCharacters a where
    unifyCharacters :: a -> TreeChars

-- | (✔)
instance ParsedCharacters FastaParseResult where
    unifyCharacters = foldr f mempty
        where
            convertSeq = V.fromList . fmap (Just . pure . pure . pure)
            f (FastaSequence n s) = insert n (convertSeq s)

-- | (✔)
instance ParsedCharacters TaxonSequenceMap where
    unifyCharacters = fmap (pure . pure)

-- | (✔)
instance ParsedCharacters FastcParseResult where
    unifyCharacters = foldl f mempty
        where
            f m (FastcSequence label symbols) = insert label (pure $ pure symbols) m

-- | (✔)
instance ParsedCharacters NewickForest where
    unifyCharacters = mergeMaps . fmap f
        where
            f :: NewickNode -> TreeChars
            f node 
              | null (descendants node) = insert name mempty mempty
              | otherwise = foldl1 (<>) $ f <$> descendants node
              where
                  name = fromMaybe "" $ newickLabel node

-- | (✔)
instance ParsedCharacters Nexus where
    unifyCharacters (Nexus (seqMap, _)) = seqMap

-- | (✔)
instance ParsedCharacters TntResult where
    unifyCharacters (Left forest) = mergeMaps $ foldl f mempty forest
      where
          f xs tree = foldl g mempty tree : xs
          g m (Index  i) = insert (show i) mempty m
          g m (Name   n) = insert n mempty m
          g m (Prefix p) = insert p mempty m
    unifyCharacters (Right (WithTaxa seqs _ []    )) = M.fromList . toList $ second tntToTheSuperSequence   <$> seqs
    -- maybe just use the seq vaiable like above and remove this case?
    unifyCharacters (Right (WithTaxa _    _ forest)) = mergeMaps $ (M.fromList . toList . fmap (second tntToTheSuperSequence)) <$> forest

-- | Coalesce the 'TaxonSequence' to the larger type 'ParsedSequences'
tntToTheSuperSequence :: TaxonSequence -> ParsedChars
tntToTheSuperSequence = V.fromList . fmap (Just . pure . f . show)
    where
        f ('[':xs) = pure <$> init xs
        f e        = pure e

-- | Takes a 'Foldable' structure of 'Map's and returns the union 'Map'
--   containing all the key value pairs. This fold is right biased with respect
--   to duplicate keys. When identical keys occur in multiple 'Map's, the value
--   occuring last in the 'Foldable' structure is returned.
mergeMaps :: (Foldable t, Ord k) => t (Map k v) -> Map k v
mergeMaps = foldl (mergeWithKey (\_ _ b -> Just b) id id) mempty

instance ParsedCharacters TCM where
    unifyCharacters _ = mempty

instance ParsedCharacters VertexEdgeRoot where
    unifyCharacters (VER _ e r) = mergeMaps $ f . buildTree <$> toList r
        where
            es = toList e
            f node 
              | null (subForest node) = insert (rootLabel node) mempty mempty
              | otherwise = foldl1 (<>) $ f <$> subForest node
            buildTree name = Node name kids
                where
                    kids = fmap (buildTree . snd) . filter ((==name) . fst) $ edgeConnection <$> es


