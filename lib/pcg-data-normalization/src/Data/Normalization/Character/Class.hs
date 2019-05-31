-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Normalization.Character.Class
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
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE ScopedTypeVariables  #-}

{-# LANGUAGE NoMonoLocalBinds     #-}

module Data.Normalization.Character.Class
  ( Identifier
  , HasNormalizedCharacters(..)
  , NormalizedCharacter(..)
  , NormalizedCharacters
  , NormalizedCharacterCollection
  ) where

import           Control.Arrow                         ((***))
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty                    (NonEmpty (..))
import qualified Data.List.NonEmpty                    as NE
import           Data.Map                              (Map)
import qualified Data.Map                              as M
import           Data.Maybe
import           Data.Normalization.Character.Internal
import           Data.Semigroup.Foldable               ()
--import           Data.Set                         (Set)
--import qualified Data.Set                         as S
--import           Data.ShortText.Custom            (intToShortText)
import           Data.String                           (IsString (fromString))
import           Data.Text.Short                       (ShortText)
--import           Data.Tree
import           Data.Vector                           (Vector)
--import qualified Data.Vector                      as V
--import           Data.Vector.Instances            ()
--import           Data.Vector.NonEmpty                  (Vector)
import qualified Data.Vector.NonEmpty                  as VNE
import qualified Data.Vector.Unboxed                   as VU
import           File.Format.Dot
import           File.Format.Fasta
import           File.Format.Fastc                     hiding (Identifier)
import           File.Format.Newick
import           File.Format.Nexus                     hiding (TaxonSequenceMap)
import           File.Format.TNT
import qualified File.Format.TNT                       as TNT
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot
import           Prelude                               hiding (zipWith)


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
class HasNormalizedCharacters a where

    getNormalizedCharacters :: a -> NormalizedCharacters


-- | (✔)
instance HasNormalizedCharacters (DotGraph GraphID) where

    getNormalizedCharacters = const mempty
{-      fromSet (const mempty) . S.map (fromString . toIdentifier) . leafNodeSet
      where
        -- Get the set of all nodes with out degree 0.
        leafNodeSet :: Ord n => DotGraph n -> Set n
        leafNodeSet = keysSet . M.filter null . dotChildMap
-}


-- | (✔)
instance HasNormalizedCharacters FastaParseResult where

    getNormalizedCharacters = foldMap f
      where
        f (FastaSequence _ s) = M.singleton (g s) (convertSeq (g s))
        convertSeq = pure . parsedDynamicCharacterFromShortText

        g = fromString . VU.toList


-- | (✔)
instance HasNormalizedCharacters FastcParseResult where

    getNormalizedCharacters = foldMap f
      where
        f (FastcSequence label symbols) = M.singleton label $ convertCharacterSequenceLikeFASTA symbols


-- | (✔)
instance HasNormalizedCharacters TaxonSequenceMap where

    getNormalizedCharacters = fmap convertCharacterSequenceLikeFASTA


-- | (✔)
instance HasNormalizedCharacters (NonEmpty NewickForest) where

    getNormalizedCharacters = const mempty
{-
      mergeMaps . foldMap1 (fmap f)
      where
        f :: NewickNode -> Map ShortText NormalizedCharacterCollection
        f node =
          case descendants node of
            []   -> M.singleton nodeName mempty
            x:xs -> foldMap1 f $ x:|xs
          where
            nodeName = fromMaybe "" $ newickLabelShort node
-}


-- | (✔)
instance HasNormalizedCharacters Nexus where

    getNormalizedCharacters (Nexus (seqMap, metadataVector) _) =
        case VNE.fromVector metadataVector of
          Nothing -> mempty
          Just mv -> M.mapMaybe (f mv) $ M.mapKeysMonotonic fromString seqMap
      where
        f mv = fmap (zipWith g mv) . VNE.fromVector

        g :: CharacterMetadata -> Character -> NormalizedCharacter
        g m e
          | not $ isAligned m = NormalizedDynamicCharacter . convert $ e
          | otherwise         = NormalizedDiscreteCharacter $ do
              v <- e                      -- Check if the element is empty
              w <- NE.nonEmpty $ toList v -- If not, coerce the Vector to a NonEmpty list
              NE.nonEmpty                 -- Then grab the first element of the Vector,
                . fmap fromString         -- making sure it is also a NonEmpty list
                . NE.head
                $ w

                -- Maybe (Vector [String])
        convert :: Character -> Maybe (NonEmpty (NonEmpty ShortText))
        convert = fmap innerConv1
          where

            innerConv1 :: Vector [String] -> NonEmpty (NonEmpty ShortText)
            innerConv1 = NE.fromList . toList . fmap innerConv2

            innerConv2 :: [String] -> NonEmpty ShortText
            innerConv2 []           =
              error "Encountered empty list of Nexus characters during conversion"
            innerConv2 [str]        = fromString str :| []
            innerConv2 (str : str') = fromString str :| fmap fromString str'



-- | (✔)
instance HasNormalizedCharacters TntResult where

    getNormalizedCharacters Left {} = mempty
{-
    getNormalizedCharacters (Left forest) = mergeMaps $ foldl' f [] forest
      where
        f xs tree = foldMap g tree : xs
        g (Index  i) = M.singleton (intToShortText i) mempty
        g (Name   n) = M.singleton (fromString n) mempty
        g (Prefix p) = M.singleton (fromString p) mempty
-}

    getNormalizedCharacters (Right (WithTaxa seqs _ forest)) =
        case forest of
          []   -> buildMapFromSeqs seqs
          x:xs -> mergeMaps $ buildMapFromSeqs <$> (x:|xs)


-- | (✔)
instance HasNormalizedCharacters TCM where

    getNormalizedCharacters = const mempty


-- | (✔)
instance HasNormalizedCharacters VertexEdgeRoot where

    getNormalizedCharacters = const mempty
{-
    getNormalizedCharacters (VER _ e r) =
        mergeMaps $ f . buildTree <$> toList r
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
-}


buildMapFromSeqs
  :: Traversable t
  => t (String, TaxonSequence)
  -> Map ShortText NormalizedCharacterCollection
buildMapFromSeqs = M.fromList . filterNothings . fmap (fromString *** tntToTheSuperSequence)
  where
    filterNothings = foldr f []
      where
        f (_, Nothing) acc = acc
        f (k, Just v ) acc = (k,v):acc

-- |
-- Coalesce the 'TaxonSequence' to the larger type 'NormalizedSequences'
tntToTheSuperSequence :: TaxonSequence -> Maybe NormalizedCharacterCollection
tntToTheSuperSequence ts =
    case ts of
      []   -> Nothing
      x:xs -> Just . VNE.fromNonEmpty $ f <$> x:|xs
  where
    f (TNT.Continuous c) = NormalizedContinuousCharacter c
    f discreteCharacter  = NormalizedDiscreteCharacter
                         . Just
                         . fmap fromString
                         . coerceDiscreteRendering
                         . show
                         $ discreteCharacter

    coerceDiscreteRendering ('[':xs) = NE.fromList $ pure <$> init xs
    coerceDiscreteRendering e        = pure e
