{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances #-}

module Test.Custom.Tree
  ( TestTree()
  , createTestTree
  ) where

import Data.Char
import Test.QuickCheck

import           Bio.Character.Dynamic.Coded
import           Bio.PhyloGraph.DAG.Class
import qualified Bio.PhyloGraph.Network           as N
import qualified Bio.PhyloGraph.Node.Encoded      as EN
import qualified Bio.PhyloGraph.Node.Final        as FN
import qualified Bio.PhyloGraph.Node.ImpliedAlign as IN
import qualified Bio.PhyloGraph.Node.Preliminary  as RN
import           Bio.PhyloGraph.Node.Referential
import           Bio.PhyloGraph.Tree.Binary
import qualified Bio.PhyloGraph.Tree.Referential  as RT
import           Bio.PhyloGraph.Tree.Rose
import           Control.Applicative                     ((<|>))
import           Control.Monad                           ((<=<))
import           Data.Alphabet
import           Data.Foldable
import           Data.IntMap                             (IntMap, insertWith)
import qualified Data.IntMap                      as IM
import           Data.IntSet                             (IntSet)
import qualified Data.IntSet                      as IS
import           Data.Key
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import qualified Data.Set                         as S
import           Data.Tree
import           Data.Vector                             (Vector)
import qualified Data.Vector                      as V
import           Prelude                          hiding (lookup)
import           Safe                                    (tailMay)

createTestTree :: Foldable t
                => Int      -- ^ Root node reference
                -> String   -- ^ Alphabet symbols
                -> t (Int, String, [Int]) -- ^ (Node Reference, sequence of dynamic characters, child nodes)
                -> TestTree
createTestTree rootRef symbols xs = TT $ unfoldTree buildTree rootRef
  where
    alphabet = constructAlphabet $ pure <$> symbols
--    mapping :: (Foldable a, Foldable c, Foldable v) => IntMap (v (c (a String)), IntSet)
    mapping = foldl' f mempty xs
      where
        f m (i, seq, adjacency) = insertWith g i (seq, IS.fromList adjacency) m
          where
            g (newSeq, lhs) (_, rhs) = (newSeq, lhs <> rhs)
    buildTree :: Int -> (TestingDecoration, [Int])
    buildTree i = (def { dEncoded = pure . encodeDynamic alphabet $ (\c -> [[c]]) <$>  seq }, otoList children)
      where
        (seq, children) = mapping ! i
    

createCherry :: String -> String -> String -> TestTree
createCherry rootCharacter leftCharacter rightCharacter = createTestTree 0 alphabet [(0,rootCharacter,[1,2]), (1,leftCharacter,[]), (2,rightCharacter,[])]
  where
    alphabet = toList $ foldMap S.fromList [rootCharacter, leftCharacter, rightCharacter]

data TestTree = TT (Tree TestingDecoration)
  deriving (Eq, Show)

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
   } deriving (Eq, Show)

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

sameRef :: TestTree -> TestTree -> Bool
sameRef (TT x) (TT y) = ref x == ref y
  where
    ref = refEquality . rootLabel

--type instance Element (Tree TestingDecoration) = TestingDecoration

instance EN.EncodedNode TestTree DynamicChar where
    getEncoded     (TT n)   = dEncoded $ rootLabel n
    setEncoded     (TT n) x = TT $ n { rootLabel = decoration { dEncoded = x } }
      where
        decoration = rootLabel n

instance FN.FinalNode TestTree DynamicChar where
    getFinal       (TT n) = dFinal   $ rootLabel n
    setFinal     x (TT n) = TT $ n { rootLabel = decoration { dFinal = x } }
      where
        decoration = rootLabel n
        
    getFinalGapped   (TT n) = dGapped  $ rootLabel n
    setFinalGapped x (TT n) = TT $ n { rootLabel = decoration { dGapped = x } }
      where
        decoration = rootLabel n

instance RN.PreliminaryNode TestTree DynamicChar where
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

instance IN.IANode TestTree where
    getHomologies     (TT n)   = dIaHomology $ rootLabel n
    setHomologies     (TT n) x = TT $ n { rootLabel = decoration { dIaHomology = x } }
      where
        decoration = rootLabel n


instance N.Network TestTree TestTree where
   -- | Not efficient but correct.
   parents (TT node) (TT tree)
     | node == tree = []
     | otherwise    = foldMap (f tree) $ subForest tree
     where
       f parent child
         | child == node = [TT parent]
         | otherwise     = foldMap (f child) $ subForest child

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
       nodes' :: [TestTree]
       nodes' = f <$> nodes
         where
           f :: TestTree -> TestTree
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
         
instance RT.ReferentialTree TestTree TestTree where
    code (TT node) (TT root) = snd $ foldl' f (0, Nothing) root
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

instance BinaryTree TestTree TestTree where
    leftChild    (TT internal) _ = fmap TT .  headMay $ subForest internal
    rightChild   (TT internal) _ = fmap TT . (headMay <=< tailMay) $ subForest internal
    verifyBinary = isNothing . findNode isNotBinaryNode
      where
        isNotBinaryNode (TT node) = (> 2) . length $ subForest node

instance RoseTree TestTree TestTree where
    parent node root = findNode isParent root
      where
        isParent (TT internal) = any (sameRef node . TT) $ subForest internal

idMatches :: Int -> TestTree -> Bool
idMatches target (TT internal) = refEquality (rootLabel internal) == target

findNode :: (TestTree -> Bool) -> TestTree -> Maybe TestTree
findNode f tree@(TT x)
  | f tree    = Just tree
  | otherwise = foldl' (<|>) Nothing $ (findNode f. TT) <$> subForest x


