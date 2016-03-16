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

import Control.Arrow ((***))
import File.Format.Fasta
import File.Format.Fastc
import File.Format.TNT
import Data.Foldable
import File.Format.Newick
import Data.Map (insert)
import qualified Data.Map as M (fromList)
import Data.Maybe
import Data.Monoid
import Data.Key
import Bio.Sequence.Parsed
import qualified Data.Vector as V

-- TODO: Make sure that pipelines don't undo and redo the conversion to treeSeqs
-- currently we pack and unpack codes, make parsers dumber in the future
class ParsedCharacters a where
    unifyCharacters :: a -> [TreeSeqs]

instance ParsedCharacters FastaParseResult where
    unifyCharacters = pure . foldr f mempty
        where
            convertSeq = V.fromList . map (Just . pure . pure . pure)
            f (FastaSequence n s) = insert n (convertSeq s)

instance ParsedCharacters TntResult where
    unifyCharacters (Left trees) = foldl f mempty trees
      where
        f xs tree = foldl g mempty tree : xs
        g m (Index  i) = insert (show i) mempty m
        g m (Name   n) = insert n mempty m
        g m (Prefix p) = insert p mempty m
    unifyCharacters (Right (WithTaxa seqs _ []))    = pure . M.fromList . toList $ (id *** tntToTheSuperSequence) <$> seqs
    unifyCharacters (Right (WithTaxa seqs _ trees)) = (M.fromList . toList . fmap (id *** tntToTheSuperSequence)) <$> trees
        
tntToTheSuperSequence :: TaxonSequence -> ParsedSequences
tntToTheSuperSequence inSeq = V.fromList $ (Just . pure . f . show) <$> inSeq
  where
    f ('[':xs) = pure <$> init xs
    f e        = pure e

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


