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
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Bio.Character.Parsed.Class
  ( ParsedCharacters(..)
  , TaxonCharacters
  ) where

import           Bio.Character.Parsed.Internal
import           Control.Arrow                    ((&&&), (***))
import           Data.Bifunctor                   (second)
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NE
import           Data.Map                         (Map, fromSet, insert, keysSet, mergeWithKey)
import qualified Data.Map                         as M
import           Data.Maybe
import           Data.Semigroup.Foldable
import           Data.Set                         (Set)
import qualified Data.Set                         as S
import           Data.Tree
import           Data.Vector.Custom               as V (fromList')
import           Data.Vector.Instances            ()
import           Data.Vector (Vector)
import           File.Format.Dot
import           File.Format.Fasta
import           File.Format.Fastc
import           File.Format.Newick
import           File.Format.Nexus                hiding (TaxonSequenceMap)
import           File.Format.TNT
import qualified File.Format.TNT                  as TNT
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot
import           Prelude                          hiding (zipWith)
import Data.String(IsString(fromString))
import Data.ShortText.Custom (intToShortText)
import Data.Text.Short(ShortText)


{-
data ParsedCharacter
   = ParsedContinuousCharacter  Double
   | ParsedDiscreteCharacter   (AmbiguityGroup String)
   | ParsedDynamicCharacter    (NonEmpty (AmbiguityGroup String))


type ParsedChars = Vector (Maybe ParsedCharacter)


type TaxonCharacters = Map String ParsedChars
-}


-- |
-- Instances provide a method to extract 'Character' sequences from raw parsed results.
-- The 'TreeSeqs' are agnostic of character data types. "Tree-only" return values from
-- files will extract the taxa labels from leaf nodes only with empty sequences.
--
-- Characters of types DNA, RNA, protein, and amino acid will *not* have thier IUPAC
-- codes translated to the apropriate groups. This abiguity group translation will
-- occur later, during the rectification process with the character metadata. Parsers
-- which produce expanded ambiguity groups for these character types will be collapsed
-- back to the IUPAC code for the ambiguity group during the type class's extraction
-- process.
--
-- It is expected that parsers will be altered to return simpler character literals for
-- time efficiency in the future.
--
-- I need to think about how this might interact with some things in Nexus, but it seems
-- to make sense. It might make verification in the parsers more difficult.
class ParsedCharacters a where

    unifyCharacters :: a -> TaxonCharacters


-- | (✔)
instance ParsedCharacters (DotGraph GraphID) where

    unifyCharacters = fromSet (const mempty) . S.map (fromString . toIdentifier) . leafNodeSet
      where
        -- Get the set of all nodes with out degree 0.
        leafNodeSet :: Ord n => DotGraph n -> Set n
        leafNodeSet = keysSet . M.filter null . dotChildMap


-- | (✔)
instance ParsedCharacters FastaParseResult where

    unifyCharacters = foldMap f
      where
        f (FastaSequence n s) = M.singleton (fromString s) (convertSeq (fromString s))
        convertSeq = pure . parsedDynamicCharacterFromShortText


-- | (✔)
instance ParsedCharacters FastcParseResult where

    unifyCharacters = foldMap f
      where
        f (FastcSequence label symbols) = M.singleton (fromString label) $ convertCharacterSequenceLikeFASTA symbols


-- | (✔)
instance ParsedCharacters TaxonSequenceMap where

    unifyCharacters = (fmap convertCharacterSequenceLikeFASTA) . M.mapKeysMonotonic fromString


-- | (✔)
instance ParsedCharacters (NonEmpty NewickForest) where

    unifyCharacters = mergeMaps . foldMap1 (fmap f)
      where
        f node
          | null (descendants node) = M.singleton nodeName mempty
          | otherwise = foldMap f $ descendants node -- foldl1 (<>) $ f <$> descendants node
          where
            nodeName = fromMaybe "" $ newickLabelShort node


-- | (✔)
instance ParsedCharacters Nexus where

    unifyCharacters (Nexus (seqMap, metadataVector) _)
      = f <$> (M.mapKeysMonotonic fromString seqMap)
      where

        f = zipWith g metadataVector

        g :: CharacterMetadata -> Character -> ParsedCharacter
        g m e
          | not $ isAligned m
              = ParsedDynamicCharacter . convert $ e
          | otherwise         = ParsedDiscreteCharacter $ do
              v <- e                      -- Check if the element is empty
              w <- NE.nonEmpty $ toList v -- If not, coerce the Vector to a NonEmpty list
              NE.nonEmpty                 -- Then grab the first element of the Vector,
                $ fmap fromString         -- making sure it is also a NonEmpty list
                . NE.head
                $ w  
                                          

                -- Maybe (Vector [String])
        convert :: Character -> Maybe (NonEmpty (NonEmpty (ShortText)))
        convert = fmap innerConv1
          where
         
            innerConv1 :: Vector [String] -> NonEmpty (NonEmpty (ShortText))
            innerConv1 = NE.fromList . toList . fmap innerConv2
    
            innerConv2 :: [String] -> NonEmpty (ShortText)
            innerConv2 []           =
              error "Encountered empty list of Nexus characters during conversion"
            innerConv2 [str]        = fromString str :| []
            innerConv2 (str : str') = fromString str :| fmap fromString str'
         


-- | (✔)
instance ParsedCharacters TntResult where

    unifyCharacters (Left forest) = mergeMaps $ foldl' f mempty forest
      where
        f xs tree = foldMap g tree : xs
        g (Index  i) = M.singleton (intToShortText i) mempty
        g (Name   n) = M.singleton (fromString n) mempty
        g (Prefix p) = M.singleton (fromString p) mempty

    unifyCharacters (Right (WithTaxa seqs _ []    ))
      = M.fromList . toList $ (fromString *** tntToTheSuperSequence)  <$> seqs
    -- maybe just use the seq vaiable like above and remove this case?
    unifyCharacters (Right (WithTaxa _    _ forest))
      = mergeMaps $ M.fromList . toList . fmap (fromString *** tntToTheSuperSequence) <$> forest


-- | (✔)
instance ParsedCharacters TCM where

    unifyCharacters _ = mempty


-- | (✔)
instance ParsedCharacters VertexEdgeRoot where

    unifyCharacters (VER _ e r) = mergeMaps $ f . buildTree <$> toList r
      where
        es = toList e
        f node
          | null (subForest node) = M.singleton (fromString . rootLabel $ node) mempty
          | otherwise = foldl1 (<>) $ f <$> subForest node
        buildTree nodeName = Node nodeName kids
          where
            kids
              = fmap (buildTree . snd)
              . filter ((==nodeName) . fst)
              $ (edgeOrigin &&& edgeTarget)
              <$> es




-- |
-- Coalesce the 'TaxonSequence' to the larger type 'ParsedSequences'
tntToTheSuperSequence :: TaxonSequence -> ParsedChars
tntToTheSuperSequence = V.fromList' . fmap f
  where
    f (TNT.Continuous c) = ParsedContinuousCharacter c
    f discreteCharacter  = ParsedDiscreteCharacter
                         . Just
                         . fmap fromString
                         . coerceDiscreteRendering
                         . show
                         $ discreteCharacter

    coerceDiscreteRendering ('[':xs) = NE.fromList $ pure <$> init xs
    coerceDiscreteRendering e        = pure e


-- |
-- Takes a 'Foldable' structure of 'Map's and returns the union 'Map'
-- containing all the key-value pairs. This fold is right biased with respect
-- to duplicate keys. When identical keys occur in multiple 'Map's, the value
-- occurring last in the 'Foldable' structure is returned.
mergeMaps :: (Foldable t, Ord k) => t (Map k v) -> Map k v
mergeMaps = foldl' (\accMap map -> M.unionWith (flip const) accMap map) mempty
