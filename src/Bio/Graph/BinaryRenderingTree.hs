------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.BinaryRenderingTree
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An intermediate tree representation of nice horizontal rendering.
--
-----------------------------------------------------------------------------

module Bio.Graph.BinaryRenderingTree where


import Control.Arrow             ((&&&))
import Data.Foldable
import Data.List.NonEmpty hiding (length, takeWhile)
import Data.Semigroup
import Prelude            hiding (head)


-- |
-- An intermediate structure for rendering directed, acyclic graphs.
data  BinaryRenderingTree
    = Leaf !String
    | Node {-# UNPACK #-} !Word !(Maybe String) !(NonEmpty BinaryRenderingTree)
    deriving (Eq, Show)


-- |
-- Get the number of leaves present in a subtree.
subtreeSize :: BinaryRenderingTree -> Word
subtreeSize (Leaf _)     = 1
subtreeSize (Node x _ _) = x


-- |
-- Render a directed, acyclic graph in a horizontal fashion. Bias larger subtrees
-- towards the bottom and smaller subtrees to the top. Apply symbolic references
-- to network nodes.
horizontalRendering :: BinaryRenderingTree -> String
horizontalRendering = fold . intersperse "\n" . go
  where
    go :: BinaryRenderingTree -> NonEmpty String
    go (Leaf label) = pure $ "─ " <> label
    go (Node _ labelMay kids) = sconcat paddedSubtrees
      where
        paddedSubtrees   = maybe prefixedSubtrees (`applyPadding` prefixedSubtrees) labelMay
        
        prefixedSubtrees :: NonEmpty (NonEmpty String)
        prefixedSubtrees = applyPrefixes alignedSubtrees

        alignedSubtrees  :: NonEmpty (NonEmpty String)
        alignedSubtrees  = applySubtreeAlignment maxSubtreeDepth <$> renderedSubtrees

        renderedSubtrees :: NonEmpty (Int, NonEmpty String)
        renderedSubtrees = fmap (prefixLength &&& id) $ go <$> sortWith subtreeSize kids

        maxSubtreeDepth  = maximum $ fst <$> renderedSubtrees

        prefixLength     = length . takeWhile (`elem` "└┌│├┤─ ") . head

    applyPadding :: String -> NonEmpty (NonEmpty String) -> NonEmpty (NonEmpty String)
    applyPadding e input =
        case input of
          v:|[]     -> applyAtCenter e pad pad v :| []
          v:|(x:xs) -> fmap (pad<>) v :| (applyAtCenter e pad pad x : fmap (fmap (pad<>)) xs)
      where
        pad   = replicate (length e) ' '

    applyPrefixes :: NonEmpty (NonEmpty String) -> NonEmpty (NonEmpty String)
    applyPrefixes = run True
      where
        run :: Bool -> NonEmpty (NonEmpty String) -> NonEmpty (NonEmpty String) 
        run True  (v:|[])     = pure $ applyAtCenter "─" " " " " v
        run False (v:|[])     = pure $ applyAtCenter "└" "│" " " v
        run True  (v:|(x:xs)) = applyPrefixAndGlue  v "┤" "┌" " " "│" (x:|xs)
        run False (v:|(x:xs)) = applyPrefixAndGlue  v "│" "├" "│" " " (x:|xs)

        applyPrefixAndGlue v glue center upper lower xs = pure (applyAtCenter center upper lower v)
                                          <> pure (pure glue)
                                          <> run False xs

    applySubtreeAlignment :: Int -> (Int, NonEmpty String) -> NonEmpty String
    applySubtreeAlignment maxLength (currLength, xs) = applyAtCenter branch pad pad xs
      where
        branch = replicate (maxLength - currLength) '─'
        pad    = replicate (maxLength - currLength) ' '

    applyAtCenter :: String -> String -> String -> NonEmpty String -> NonEmpty String
    applyAtCenter center     _     _ (x:|[]) = (center<>x) :| []
    applyAtCenter center upper lower (x:|xs) = ( upper<>x) :| snd (foldr f (False, []) xs)
      where
        f :: String -> (Bool, [String]) -> (Bool, [String])
        f str (crossedMidPoint, acc) =
          case str of
            h:_ | not crossedMidPoint && h `notElem` "└┌│├ " -> ( True, (center<>str):acc)
            _   | crossedMidPoint                            -> ( True, ( upper<>str):acc)
                | otherwise                                  -> (False, ( lower<>str):acc)
