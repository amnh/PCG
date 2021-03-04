-----------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Types.Report.ImpliedAlignment
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Output a FASTA file containing the implied alignment of each character.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module PCG.Command.Report.ImpliedAlignment
  ( impliedAlignmentOutputs
  ) where

--import           Analysis.Distance
import           Bio.Character
import           Bio.Character.Decoration.Additive
import           Bio.Character.Decoration.Continuous
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Decoration.Fitch
import           Bio.Character.Decoration.Metric
import           Bio.Character.Encodable
import           Bio.Graph
import           Bio.Graph.LeafSet
import           Bio.Graph.Node
import           Bio.Metadata
import           Bio.Sequence
--import           Bio.Sequence.Metadata
import           Control.Arrow
import           Control.Lens
--import           Control.Lens.Operators              ((^.))
import           Data.Alphabet
import           Data.Alphabet.IUPAC
--import           Data.Bimap                          (Bimap)
import qualified Data.Bimap                          as BM
import           Data.Bits
import           Data.CharacterName
import           Data.Coerce
--import           Data.FileSource
import           Data.Foldable
import           Data.Key
--import           Data.List                           (intersperse)
import           Data.List.NonEmpty                  (NonEmpty((:|)))
import qualified Data.List.NonEmpty                  as NonEmpty
import           Data.List.Utility
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
--import           Data.Matrix.Unboxed                 (Matrix)
--import qualified Data.Matrix.Unboxed                 as Matrix
import           Data.Maybe
import           Data.MonoTraversable
import           Data.NodeLabel
import           Data.Semigroup.Foldable             (Foldable1(..))
import           Data.String
import           Data.Text.Lazy                      (Text)
import qualified Data.Text.Lazy                      as Text
--import           Data.Text.Lazy.Builder              (Builder)
import qualified Data.Text.Lazy.Builder              as Builder
--import qualified Data.Text.Lazy.Builder.RealFloat    as Builder
--import           Data.Text.Short                     (ShortText)
import           Data.Vector                         (Vector)
--import qualified Data.Vector                         as Vector
import qualified Data.Vector.NonEmpty                as NEV
import           Prelude                             hiding (filter)
import           TextShow                            hiding (fromString)
--import           TextShow.Custom


-- |
-- Gets the serialized streams of the implied alignment for each chearacter.
impliedAlignmentOutputs :: DecoratedCharacterResult -> Map CharacterName Text
impliedAlignmentOutputs solution = mempty
{-
    Map.mapKeysMonotonic fst $ mapWithKey renderAlignment charMap
  where
    meta    = view _columnMetadata . extractSolution $ solution
    leaves  = fromLeafSet $ view leafSet solution
    charMap = gatherImpliedAlignments leaves meta

    renderAlignment (_,a) m = generateImpliedAlignment a m
-}


-- |
-- Collect the Implied Alignments for each character
gatherImpliedAlignments
  :: Vector
       (PhylogeneticNode
       (CharacterSequence
         (ContinuousOptimizationDecoration ContinuousCharacter)
         (FitchOptimizationDecoration StaticCharacter)
         (AdditiveOptimizationDecoration StaticCharacter)
         (SankoffOptimizationDecoration StaticCharacter)
         (SankoffOptimizationDecoration StaticCharacter)
         (DynamicDecorationDirectOptimization DynamicCharacter))
       NodeLabel)
  -> MetadataSequence m
  -> Map (CharacterName, Alphabet String) (Map NodeLabel DynamicCharacter)
gatherImpliedAlignments l meta = iaMappings
  where
    iaMappings = foldMapWithKey f metaTags
      where
        getMetaTag = (^. characterName) &&& (^. characterAlphabet)

        f i   block = foldMapWithKey (g i) block
        g i j cMeta = Map.singleton (getMetaTag cMeta) $ foldMap (h i j) charSeqs
        h i j (label, seqs) = Map.singleton label . (^. impliedAlignment) . runIdentity $ seqs ! i ! j

    -- coerce to use a type amenable to the characterDistanceMatrix
    leaves :: Vector (DecoratedCharacterNode Identity)
    leaves = coerce l

    metaTags
      :: NEV.Vector (Vector (DynamicCharacterMetadataDec AmbiguityGroup))
    metaTags = fmap (view dynamicBin) . view blockSequence $ meta

    -- Get the character sequences from the leaf nodes.
    charSeqs
      :: Vector
           ( NodeLabel
           , NEV.Vector (Vector (Identity (DynamicDecorationDirectOptimization DynamicCharacter)))
           )
{-
    charSeqs
      :: Vector
           ( NodeLabel
           , CharacterSequence
               (Identity (ContinuousOptimizationDecoration ContinuousCharacter))
               (Identity (FitchOptimizationDecoration          StaticCharacter))
               (Identity (AdditiveOptimizationDecoration       StaticCharacter))
               (Identity (SankoffOptimizationDecoration        StaticCharacter))
               (Identity (SankoffOptimizationDecoration        StaticCharacter))
               (Identity (DynamicDecorationDirectOptimization DynamicCharacter))
           )
-}
    charSeqs = (view _nodeDecorationDatum &&& fmap (view dynamicBin) . view blockSequence . view _characterSequence . NonEmpty.head . view _resolutions) <$> leaves


generateImpliedAlignment :: Alphabet String -> Map NodeLabel DynamicCharacter -> Text
generateImpliedAlignment alphabet = Builder.toLazyText . foldMapWithKey renderCharacterAlignment
  where
    renderCharacterAlignment :: NodeLabel -> DynamicCharacter -> Builder
    renderCharacterAlignment name char = fold
        [ "> ", Builder.fromLazyText $ nodeLabelToLazyText name, "\n"
        , renderCharacter alphabet char, "\n"
        ]


-- |
-- Show an 'EncodableStream' by decoding it with its corresponding alphabet.
renderCharacter
  :: Alphabet String
  -> DynamicCharacter
  -> Builder
renderCharacter alphabet xs
  | olength xs == 0 = mempty
  | otherwise       =
      let shownElems = renderElement alphabet . getMedian <$> otoList xs
      in  if   any (\e -> olength e > 1) shownElems
          then Builder.fromLazyText $ Text.unwords shownElems
           -- All elements were rendered as a single character.
          else foldMap Builder.fromLazyText shownElems


-- |
-- Show an 'EncodableStreamElement' by decoding it with its corresponding alphabet.
renderElement :: Alphabet String -> AmbiguityGroup -> Text
renderElement alphabet e
  |   noBits == e = "<Empty Character>"
  |  allBits == e = "?"
  | otherwise     = fromString . renderAmbiguity $ toIUPAC symbols
  where
    noBits  = e `xor` e
    allBits = complement noBits
    symbols = decodeElement alphabet e
    renderAmbiguity amb =
      let (x:|xs) = toNonEmpty amb
      in  case xs of
            [] -> x
            _  -> let bases = x:xs
                  in case invariantTransformation length bases of
                       Just 1 -> "[" <> concat  bases <> "]"
                       _      -> "[" <> unwords bases <> "]"

    toIUPAC :: NonEmpty String -> NonEmpty String
    toIUPAC x
      | isAlphabetDna       alphabet = fromMaybe x $ x `BM.lookup` BM.twist iupacToDna
      | isAlphabetRna       alphabet = fromMaybe x $ x `BM.lookup` BM.twist iupacToRna
      | isAlphabetAminoAcid alphabet = fromMaybe x $ x `BM.lookup` BM.twist iupacToAminoAcid
      | otherwise                    = x
