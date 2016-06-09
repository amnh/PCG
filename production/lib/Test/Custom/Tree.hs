{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances #-}

module Test.Custom.Tree
  ( SimpleTree()
  , createBinary
  , createCherry
  , createSimpleTree
  ) where

import           Bio.Character.Dynamic.Coded
import qualified Bio.PhyloGraph.Network           as N
import qualified Bio.PhyloGraph.Node.Encoded      as EN
import qualified Bio.PhyloGraph.Node.Final        as FN
import qualified Bio.PhyloGraph.Node.ImpliedAlign as IN
import qualified Bio.PhyloGraph.Node.Preliminary  as RN
import           Bio.PhyloGraph.Node.Referential ()
import           Bio.PhyloGraph.Tree.Binary
import qualified Bio.PhyloGraph.Tree.Referential  as RT
import           Bio.PhyloGraph.Tree.Rose
import           Control.Applicative                     ((<|>))
import           Control.Monad                           ((<=<))
import           Data.Alphabet
import           Data.Bifunctor                          (second)
import           Data.BitVector                          (width)
import           Data.Foldable
import           Data.IntMap                             (IntMap, insertWith)
import qualified Data.IntMap                      as IM
import qualified Data.IntSet                      as IS
import           Data.Key
import           Data.List                               (intercalate)
import           Data.List.Utility                       (chunksOf)
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import qualified Data.Set                         as S
import           Data.Tree
import           Data.Vector                             (Vector)
import           Prelude                          hiding (lookup)
import           Safe                                    (tailMay)
import           Test.QuickCheck

createSimpleTree :: Foldable t
               => Int      -- ^ Root node reference
               -> String   -- ^ Alphabet symbols
               -> t (Int, String, [Int]) -- ^ (Node Reference, sequence of dynamic characters, child nodes)
               -> SimpleTree
createSimpleTree rootRef symbols xs = TT . setRefIds $ unfoldTree buildTree rootRef
  where
    alphabet = constructAlphabet $ pure <$> symbols
--    mapping :: (Foldable a, Foldable c, Foldable v) => IntMap (v (c (a String)), IntSet)
    mapping = foldl' f mempty xs
      where
        f m (i, strChar, adjacency) = insertWith g i (strChar, IS.fromList adjacency) m
          where
            g (newSeq, lhs) (_, rhs) = (newSeq, lhs <> rhs)
    buildTree :: Int -> (TestingDecoration, [Int])
    buildTree i = (def { dEncoded = pure . encodeDynamic alphabet $ (\c -> [[c]]) <$> strChar }, otoList children)
      where
        (strChar, children) = mapping ! i

setRefIds :: Tree TestingDecoration -> Tree TestingDecoration
setRefIds = snd . f 0
      where
        f :: Int -> Tree TestingDecoration -> (Int, Tree TestingDecoration)
        f counter root = (counter', root') 
          where
            root' = Node decoration' children'
            decoration' = (rootLabel root) { refEquality = counter }
            (counter', children') = foldr g (counter + 1, []) $ subForest root
            g e (n, ys) = second (:ys) $ f n e
    
createCherry :: String -> String -> String -> SimpleTree
createCherry rootCharacter leftCharacter rightCharacter = createSimpleTree 0 alphabet [(0,rootCharacter,[1,2]), (1,leftCharacter,[]), (2,rightCharacter,[])]
  where
    alphabet = toList $ foldMap S.fromList [rootCharacter, leftCharacter, rightCharacter]

createBinary :: (Show (t String), Foldable t) => t String -> SimpleTree
createBinary leafCharacters = TT . setRefIds . createBinary' $ createCherry' <$> chunksOf 2 leafCharacters
  where
    symbols  = toList $ foldMap S.fromList leafCharacters
    alphabet = constructAlphabet $ pure <$> symbols

    strToLeaf :: String -> Tree TestingDecoration
    strToLeaf str = Node (def { dEncoded = pure . encodeDynamic alphabet $ (\c -> [[c]]) <$> str }) []

    createCherry' :: [String] -> Tree TestingDecoration
    createCherry' [x] = strToLeaf x
    createCherry' xs  = Node def (strToLeaf <$> xs)

    createBinary' :: [Tree TestingDecoration] -> Tree TestingDecoration
    createBinary' [x] = x
    createBinary' xs  = createBinary' $ f <$> chunksOf 2 xs
      where
        f [y] = y
        f ys  = Node def ys
    

data SimpleTree = TT (Tree TestingDecoration)
  deriving (Eq)

data TestingDecoration
   = Decorations
   { dEncoded     :: Vector DynamicChar
   , dFinal       :: Vector DynamicChar
   , dGapped      :: Vector DynamicChar
   , dPreliminary :: Vector DynamicChar
   , dAligned     :: Vector DynamicChar
   , dTemporary   :: Vector DynamicChar
   , dLocalCost   :: Double
   , dTotalCost   :: Double
   , dIaHomology  :: IN.HomologyTrace
   , refEquality  :: Int
   } deriving (Eq)

def :: TestingDecoration
def = Decorations
    { dEncoded     = mempty
    , dFinal       = mempty
    , dGapped      = mempty
    , dPreliminary = mempty
    , dAligned     = mempty
    , dTemporary   = mempty
    , dLocalCost   = 0.0
    , dTotalCost   = 0.0
    , dIaHomology  = mempty
    , refEquality  = -1
    }

sameRef :: SimpleTree -> SimpleTree -> Bool
sameRef (TT x) (TT y) = ref x == ref y
  where
    ref = refEquality . rootLabel

instance Show SimpleTree where
  show (TT x) = drawTree $ show <$> x

instance Show TestingDecoration where
  show decoration = intercalate "\n" $ catMaybes renderedDecorations
    where
      renderedDecorations =
        [ g "Encoded    " <$> f dEncoded
        , g "Ungapped   " <$> f dFinal
        , g "Gapped     " <$> f dGapped
        , g "Preliminary" <$> f dPreliminary
        , g "Aligned    " <$> f dAligned
        , g "Temporary  " <$> f dTemporary
        ]
      f x = renderDynamicCharacter <$> headMay (x decoration)
      g prefix shown = prefix <> ": " <> shown --intercalate "\n" $ (prefix <> ": " <> y) : (("  " <>) <$> zs)
--        where
--          (x:y:zs) = lines shown :: [String]

renderDynamicCharacter :: DynamicChar -> String
renderDynamicCharacter char
  | onull char = ""
  | otherwise  = concatMap f $ decodeDynamic alphabet char
  where
    symbolCount = width $ char `indexChar` 0
    symbols     = take symbolCount arbitrarySymbols
    alphabet    = constructAlphabet symbols
    f :: [String] -> String
    f [x] = x
    f ambiguityGroup = "[" <> concat ambiguityGroup <> "]"

arbitrarySymbols :: [String]
arbitrarySymbols = fmap pure . ('-' :) $ ['0'..'9'] <> ['A'..'Z'] <> ['a'..'z']
    
instance Arbitrary SimpleTree where
  -- | Arbitrary Cherry
    arbitrary = do
      symbols  <- sublistOf $ ['0'..'9'] <> ['A'..'Z'] <> ['a'..'z']
      rootSeq  <- sublistOf symbols 
      leftSeq  <- sublistOf symbols 
      rightSeq <- sublistOf symbols
      pure $ createSimpleTree 0 symbols [(0,rootSeq,[1,2]),(1,leftSeq,[]),(2,rightSeq,[])]    
    
type instance Element SimpleTree = SimpleTree

treeFold x@(TT root) = (x :) . concatMap (treeFold . TT) $ subForest root

instance MonoFoldable SimpleTree where
    {-# INLINE ofoldMap #-}
    ofoldMap f = foldr (mappend . f) mempty . treeFold

    {-# INLINE ofoldr #-}
    ofoldr f e = foldr f e . treeFold

    {-# INLINE ofoldl' #-}
    ofoldl' f e = foldl' f e . treeFold

    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex f = foldr1 f . treeFold

    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex' f = foldl1 f . treeFold

instance EN.EncodedNode SimpleTree DynamicChar where
    getEncoded     (TT n)   = dEncoded $ rootLabel n
    setEncoded     (TT n) x = TT $ n { rootLabel = decoration { dEncoded = x } }
      where
        decoration = rootLabel n

instance FN.FinalNode SimpleTree DynamicChar where
    getFinal       (TT n) = dFinal   $ rootLabel n
    setFinal     x (TT n) = TT $ n { rootLabel = decoration { dFinal = x } }
      where
        decoration = rootLabel n
        
    getFinalGapped   (TT n) = dGapped  $ rootLabel n
    setFinalGapped x (TT n) = TT $ n { rootLabel = decoration { dGapped = x } }
      where
        decoration = rootLabel n

instance RN.PreliminaryNode SimpleTree DynamicChar where
    getPreliminary   (TT n) = dPreliminary $ rootLabel n
    setPreliminary x (TT n) = TT $ n { rootLabel = decoration { dPreliminary = x } }
      where
        decoration = rootLabel n

    getPreliminaryAlign (TT n) = dAligned $ rootLabel n
    setAlign          x (TT n) = TT $ n { rootLabel = decoration { dAligned = x } }
      where
        decoration = rootLabel n

    getTemporary        (TT n) = dTemporary $ rootLabel n
    setTemporary      x (TT n) = TT $ n { rootLabel = decoration { dTemporary = x } }
      where
        decoration = rootLabel n

    getLocalCost        (TT n) = dLocalCost $ rootLabel n
    setLocalCost      x (TT n) = TT $ n { rootLabel = decoration { dLocalCost = x } }
      where
        decoration = rootLabel n

    getTotalCost        (TT n) = dTotalCost $ rootLabel n
    setTotalCost      x (TT n) = TT $ n { rootLabel = decoration { dTotalCost = x } }
      where
        decoration = rootLabel n

instance IN.IANode SimpleTree where
    getHomologies     (TT n)   = dIaHomology $ rootLabel n
    setHomologies     (TT n) x = TT $ n { rootLabel = decoration { dIaHomology = x } }
      where
        decoration = rootLabel n


instance N.Network SimpleTree SimpleTree where
   -- | Not efficient but correct.
   parents (TT node) (TT tree)
     | node == tree = []
     | otherwise    = foldMap (f tree) $ subForest tree
     where
       f parentNode childNode
         | childNode == node = [TT parentNode]
         | otherwise     = foldMap (f childNode) $ subForest childNode

   root tree = tree

   children (TT x)  _ = TT <$> subForest x

   numNodes (TT x) = length x

   addNode _ _ = error "addNode called on a TestingTree. Not implements, don't call it!" -- Just don't call this!

   update (TT root) nodes = TT $ modifyTopology root'
     where
       -- Step 1: We apply the new decorations to all the nodes in the original tree
       root' :: Tree TestingDecoration 
       root' = modifyDecoration <$> root
         where
            modifyDecoration :: TestingDecoration -> TestingDecoration
            modifyDecoration decoration =
              case find (idMatches (refEquality decoration)) nodes of
                Nothing     -> decoration
                Just (TT x) -> rootLabel x

       -- Step 2: We apply the new decorations to the subtrees in the input list of updated nodes
       nodes' :: [SimpleTree]
       nodes' = f <$> nodes
         where
           f :: SimpleTree -> SimpleTree
           f node@(TT internal) = TT $ internal { rootLabel = decoration', subForest = children' }
             where
               children'   = ((\(TT x) -> x) . f . TT) <$> subForest internal
               decoration' =
                 case findNode (sameRef node) (TT root') of
                   Nothing     -> rootLabel internal 
                   Just (TT x) -> rootLabel x
       -- Step 3: We rebuild the tree applying the updated subtrees to the existing topology
       modifyTopology :: Tree TestingDecoration -> Tree TestingDecoration
       modifyTopology = unfoldTree f
         where
           f :: Tree TestingDecoration -> (TestingDecoration, [Tree TestingDecoration])
           f node = (rootLabel node, children')
             where
               children' = subForest . maybe node (\(TT x) -> x) $ find (sameRef (TT node)) nodes'
         
instance RT.ReferentialTree SimpleTree SimpleTree where
    getNodeIdx (TT node) (TT root) = snd $ foldl' f (0, Nothing) root
      where
        target = refEquality $ rootLabel node
        f :: (Int, Maybe Int) -> TestingDecoration -> (Int, Maybe Int)
        f (counter, done) e
          | isJust done             = (counter    , done        )
          | refEquality e == target = (counter    , Just counter)
          | otherwise               = (counter + 1, Nothing     )
        
    getNthNode tree@(TT root) pos =
        case foldl' f (0, Nothing) root of
          (outerBound, Nothing         ) -> error    $ mconcat ["Could not get node at position ", show pos, "! Valid range is [0,", show $ outerBound - 1, "]."]
          (_         , Just decoration ) -> fromJust $ findNode (idMatches (refEquality decoration)) tree
      where
        f (counter, found) e
          | isJust found   = (counter    , found   )
          | counter == pos = (counter + 1, Just e  )
          | otherwise      = (counter + 1, Nothing )

instance BinaryTree SimpleTree SimpleTree where
    leftChild    (TT internal) _ = fmap TT .  headMay $ subForest internal
    rightChild   (TT internal) _ = fmap TT . (headMay <=< tailMay) $ subForest internal
    verifyBinary = isNothing . findNode isNotBinaryNode
      where
        isNotBinaryNode (TT node) = (> 2) . length $ subForest node

instance RoseTree SimpleTree SimpleTree where
    parent node = findNode isParent
      where
        isParent (TT internal) = any (sameRef node . TT) $ subForest internal

idMatches :: Int -> SimpleTree -> Bool
idMatches target (TT internal) = refEquality (rootLabel internal) == target

findNode :: (SimpleTree -> Bool) -> SimpleTree -> Maybe SimpleTree
findNode f tree@(TT x)
  | f tree    = Just tree
  | otherwise = foldl' (<|>) Nothing $ (findNode f. TT) <$> subForest x


