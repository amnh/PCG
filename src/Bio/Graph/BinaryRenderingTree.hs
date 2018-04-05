module Bio.Graph.BinaryRenderingTree where


import Control.Arrow             ((&&&), (***))
import Data.Foldable
import Data.List.NonEmpty hiding (length)
import Data.Semigroup
import Prelude            hiding (head, splitAt)

data  BinaryRenderingTree
    = Leaf String
    | Node Word Word (Maybe String) (NonEmpty BinaryRenderingTree)
    deriving (Eq)


subtreeSize :: BinaryRenderingTree -> Word
subtreeSize (Leaf x)       = 1
subtreeSize (Node _ x _ _) = x


subtreeDepth :: BinaryRenderingTree -> Word
subtreeDepth (Leaf x)     = 0
subtreeDepth (Node x _ _ _) = x


horizontalRendering :: BinaryRenderingTree -> String
horizontalRendering = fold . intersperse "\n" . go
  where
    go :: BinaryRenderingTree -> NonEmpty String
    go (Leaf label) = pure $ "─ " <> label
    go (Node _ _ labelMay kids) = sconcat paddedSubtrees
      where
        paddedSubtrees   = applyPadding labelMay prefixedSubtrees
        prefixedSubtrees = applyPrefixes True alignedSubtrees

        alignedSubtrees  :: NonEmpty (NonEmpty String)
        alignedSubtrees  = (\(i, xs) -> let branch = replicate (maxSubtreeDepth - i) '─' in (branch <>) <$>  xs) <$> renderedSubtrees

        renderedSubtrees :: NonEmpty (Int, NonEmpty String)
        renderedSubtrees = fmap (length . head &&& id) $ go <$> sortWith subtreeSize kids

        maxSubtreeDepth  = maximum $ fst <$> renderedSubtrees


    applyPrefixes True  (x:|[]) = pure $ "─" <> x
    applyPrefixes False (x:|[]) = pure $ "└" <> x
    applyPrefixes True  (x:|(y:xs)) = pure ("┌" <> x) <> applyPrefixes False (y:|xs)
    applyPrefixes False (x:|(y:xs)) = pure ("├" <> x) <> applyPrefixes False (y:|xs)

    applyAtCenter e xs = 
        case xs of
          x:|[] -> xs
          _ -> intersperse pad pref  <> pure (maybe "" show labelMay <> "┤") <> intersperse pad suff 
      where
        pad = let i = length (show x) in replicate i ' '
        mid = length stream `div` 2
        (pref, suff) = (fromList *** fromList) $ splitAt mid stream

        
    applyPadding labelMay stream =
        case stream of
          x:|[] -> stream
          _ -> intersperse pad pref  <> pure (maybe "" show labelMay <> "┤") <> intersperse pad suff 
      where
        pad = maybe "│" (\x -> let i = length (show x) in replicate i ' ' <> "│") labelMay
        mid = length stream `div` 2
        (pref,suff) = (fromList *** fromList) $ splitAt mid stream
