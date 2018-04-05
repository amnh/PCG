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
    go (Node _ _ labelMay kids) = sconcat prefixedSubtrees-- paddedSubtrees
      where
--        paddedSubtrees   = applyPadding labelMay prefixedSubtrees

        prefixedSubtrees  :: NonEmpty (NonEmpty String)
        prefixedSubtrees = applyPrefixes  alignedSubtrees

        alignedSubtrees  :: NonEmpty (NonEmpty String)
        alignedSubtrees  = (\(i, xs) -> let branch = replicate (maxSubtreeDepth - i) '─' in (branch <>) <$>  xs) <$> renderedSubtrees

        renderedSubtrees :: NonEmpty (Int, NonEmpty String)
        renderedSubtrees = fmap (length . head &&& id) $ go <$> sortWith subtreeSize kids

        maxSubtreeDepth  = maximum $ fst <$> renderedSubtrees

{-
    applyPrefixes True  (x:|[]) = pure $ "─" <> x
    applyPrefixes False (x:|[]) = pure $ "└" <> x
    applyPrefixes True  (x:|(y:xs)) = pure ("┌" <> x) <> applyPrefixes False (y:|xs)
    applyPrefixes False (x:|(y:xs)) = pure ("├" <> x) <> applyPrefixes False (y:|xs)
-}

    applyPrefixes :: NonEmpty (NonEmpty String) -> NonEmpty (NonEmpty String)
    applyPrefixes = go True
      where
        go :: Bool -> NonEmpty (NonEmpty String) -> NonEmpty (NonEmpty String) 
        go True  (v:|[]) = pure $ applyAtCenter '─' ' ' ' ' v
        go False (v:|[]) = pure $ applyAtCenter '└' '│' ' ' v
        go True  (v:|(x:xs)) = pure (applyAtCenter '┌' ' ' '│' v) <> go False (x:|xs)
        go False (v:|(x:xs)) = pure (applyAtCenter '├' '│' '│' v) <> go False (x:|xs)

    applyAtCenter :: Char -> Char -> Char -> NonEmpty String -> NonEmpty String
    applyAtCenter center upper lower xs =
        case xs of
          x:|[] -> (center : x) :| []
          _ -> fmap (upper:) pref <> pure (center:midPoint) <> fmap (lower:) (fromList suff)
      where
        mid = length xs `div` 2
        (pref, midPoint:|suff) = (fromList *** fromList) $ splitAt mid xs
{--}
        
    applyPadding labelMay stream =
        case stream of
          x:|[] -> stream
          _ -> intersperse pad pref  <> pure (maybe "" show labelMay <> "┤") <> intersperse pad suff 
      where
        pad = maybe "│" (\x -> let i = length (show x) in replicate i ' ' <> "│") labelMay
        mid = length stream `div` 2
        (pref,suff) = (fromList *** fromList) $ splitAt mid stream
