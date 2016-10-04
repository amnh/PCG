{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances #-}

module Test.Custom.Tree
  ( SimpleTree()
  , createBinary
  , createCherry
  , createSimpleTree
  , simpleTreeCharacterDecorationEqualityAssertion
  ) where

import           Bio.Character.Dynamic
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
import           Data.Foldable
import           Data.IntMap                             (insertWith)
import qualified Data.IntSet                      as IS
import           Data.Key                         hiding (zipWith)
import           Data.List                               (intercalate)
import           Data.List.NonEmpty                      (NonEmpty((:|)))
import qualified Data.List.NonEmpty               as NE
import           Data.List.Utility                       (chunksOf)
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Ord                                (comparing)
import qualified Data.Set                         as S
import           Data.Tree
import           Data.Vector                             (Vector)
import           Prelude                          hiding (lookup)
import           Safe                                    (tailMay)
import           Test.QuickCheck
import           Test.Tasty.HUnit

createSimpleTree :: Foldable t
               => Int      -- ^ Root node reference
               -> String   -- ^ Alphabet symbols
               -> t (Int, String, [Int]) -- ^ (Node Reference, sequence of dynamic characters, child nodes)
               -> SimpleTree
createSimpleTree rootRef symbols xs = TT . setRefIds $ unfoldTree buildTree rootRef
  where
    alphabet = fromSymbols $ pure <$> symbols
--    mapping :: (Foldable a, Foldable c, Foldable v) => IntMap (v (c (a String)), IntSet)
    mapping = foldl' f mempty xs
      where
        f m (i, strChar, adjacency) = insertWith g i (strChar, IS.fromList adjacency) m
          where
            g (newSeq, lhs) (_, rhs) = (newSeq, lhs <> rhs)
    buildTree :: Int -> (TestingDecoration, [Int])
    buildTree i = (def { dEncoded = encodedSequence, suppliedAlphabet = Just alphabet }, otoList children)
      where
        encodedSequence =
          if   null strChar
          then mempty
          else pure . encodeStream alphabet . NE.fromList $ (\c -> [c]:|[]) <$> strChar
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

createBinary :: Foldable t => t String -> SimpleTree
createBinary leafCharacters = TT . setRefIds . createBinary' $ createCherry' <$> chunksOf 2 leafCharacters
  where
    symbols  = toList $ foldMap S.fromList leafCharacters
    alphabet = fromSymbols $ pure <$> symbols

    strToLeaf :: String -> Tree TestingDecoration
    strToLeaf str = Node (def { dEncoded = pure . encodeStream alphabet . NE.fromList $ (\c -> [c]:|[]) <$> str }) []

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
   { dEncoded          :: Vector DynamicChar
   , dSingle           :: Vector DynamicChar
   , dFinal            :: Vector DynamicChar
   , dGapped           :: Vector DynamicChar
   , dPreliminary      :: Vector DynamicChar
   , dLeftAlignment    :: Vector DynamicChar
   , dRightAlignment   :: Vector DynamicChar
   , dAligned          :: Vector DynamicChar
   , dTemporary        :: Vector DynamicChar
   , dLocalCost        :: Double
   , dTotalCost        :: Double
   , dIaHomology       :: IN.HomologyTrace
   , dImpliedAlignment :: Vector DynamicChar
   , refEquality       :: Int
   , suppliedAlphabet  :: Maybe (Alphabet String)
   } deriving (Eq)

def :: TestingDecoration
def = Decorations
    { dEncoded          = mempty
    , dSingle           = mempty
    , dFinal            = mempty
    , dGapped           = mempty
    , dPreliminary      = mempty
    , dLeftAlignment    = mempty
    , dRightAlignment   = mempty
    , dAligned          = mempty
    , dTemporary        = mempty
    , dLocalCost        = 0.0
    , dTotalCost        = 0.0
    , dIaHomology       = mempty
    , dImpliedAlignment = mempty
    , refEquality       = -1
    , suppliedAlphabet  = Nothing
    }

sameRef :: SimpleTree -> SimpleTree -> Bool
sameRef x y = nodeRef x == nodeRef y

nodeRef :: SimpleTree -> Int
nodeRef (TT x) = refEquality $ rootLabel x

instance Show SimpleTree where
  show (TT x) = drawTreeMultiLine $ show <$> x

instance Show TestingDecoration where
  show decoration = intercalate "\n" $ catMaybes renderings
    where
      renderings = mconcat [renderedId, renderedCosts, renderedDecorations]
      
      renderedId = pure . pure $ "Node ( " <> show (refEquality decoration) <> " )"
      renderedCosts =
        [  pure $ "LocalCost   " <> show (dLocalCost decoration)
        ,  pure $ "TotalCost   " <> show (dTotalCost decoration)
--        [  ("LocalCost   " <>)  <$> h dLocalCost
--        ,  ("TotalCost   " <>)  <$> h dTotalCost
        ]
      renderedDecorations =
        [ g "Encoded                   " <$> f dEncoded
        , g "Single                    " <$> f dSingle
        , g "Final Ungapped            " <$> f dFinal
        , g "Final Gapped              " <$> f dGapped
        , g "Preliminary Ungapped      " <$> f dPreliminary
        , g "Preliminary Gapped        " <$> f dAligned
        , g "Left  Child-wise Alignment" <$> f dLeftAlignment
        , g "Right Child-wise Alignment" <$> f dRightAlignment
        , g "Implied Alignment         " <$> f dImpliedAlignment
        ]
      alphabetToken = suppliedAlphabet decoration
      f x = renderDynamicCharacter alphabetToken <$> headMay (x decoration)
      g prefix shown = prefix <> ": " <> shown --intercalate "\n" $ (prefix <> ": " <> y) : (("  " <>) <$> zs)
--        where
--          (x:y:zs) = lines shown :: [String]
{-
      h x
        | x decoration == 0.0 = Nothing
        | otherwise           = Just . show $ x decoration
-}

-- | Neat 2-dimensional drawing of a tree.
drawTreeMultiLine :: Tree String -> String
drawTreeMultiLine = unlines . draw

draw :: Tree String -> [String]
draw (Node x xs) = lines x <> drawSubTrees xs
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
      "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
      "|" : shift "+- " "|  " (draw t) <> drawSubTrees ts
    shift first other = Prelude.zipWith (<>) (first : repeat other)

renderDynamicCharacter :: Maybe (Alphabet String) -> DynamicChar -> String
renderDynamicCharacter alphabetMay char
  | onull char = ""
  | otherwise  = concatMap (f . toList) $ decodeStream alphabet char
  where
    symbolCount     = stateCount $ char `indexStream` 0
    symbols         = take symbolCount arbitrarySymbols
    defaultAlphabet = fromSymbols symbols
    alphabet        = fromMaybe defaultAlphabet alphabetMay
    f :: [String] -> String
    f [x] = x
    f ambiguityGroup = "[" <> concat ambiguityGroup <> "]"

arbitrarySymbols :: [String]
arbitrarySymbols = fmap pure . ('-' :) $ ['0'..'9'] <> ['A'..'Z'] <> ['a'..'z']
    
instance Arbitrary SimpleTree where
  -- | Arbitrary Cherry
    arbitrary = do
      let defaultSymbols         = ['0'..'9'] <> ['A'..'Z'] <> ['a'..'z']
      alphabetLength             <- choose (1, length defaultSymbols) -- Inclusive bounds
      let defaultAlphabetSymbols = take alphabetLength defaultSymbols
      leafNodeCount              <- choose (2, 16) -- Inclusive bounds
      let leafNodeCharGen        = listOf1 (elements defaultAlphabetSymbols)
      createBinary <$> vectorOf leafNodeCount leafNodeCharGen

type instance Element SimpleTree = SimpleTree


treeFold :: SimpleTree -> [SimpleTree]
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

    getSingle       (TT n) = dSingle   $ rootLabel n
    setSingle     x (TT n) = TT $ n { rootLabel = decoration { dSingle = x } }
      where
        decoration = rootLabel n

instance RN.PreliminaryNode SimpleTree DynamicChar where
    getPreliminaryUngapped   (TT n) = dPreliminary $ rootLabel n
    setPreliminaryUngapped x (TT n) = TT $ n { rootLabel = decoration { dPreliminary = x } }
      where
        decoration = rootLabel n

    getPreliminaryGapped   (TT n) = dAligned $ rootLabel n
    setPreliminaryGapped x (TT n) = TT $ n { rootLabel = decoration { dAligned = x } }
      where
        decoration = rootLabel n

    getLeftAlignment    (TT n) = dLeftAlignment $ rootLabel n
    setLeftAlignment  x (TT n) = TT $ n { rootLabel = decoration { dLeftAlignment = x } }
      where
        decoration = rootLabel n

    getRightAlignment   (TT n) = dRightAlignment $ rootLabel n
    setRightAlignment x (TT n) = TT $ n { rootLabel = decoration { dRightAlignment = x } }
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

instance IN.IANode' SimpleTree DynamicChar where
    getHomologies'    (TT n)   = dImpliedAlignment $ rootLabel n
    setHomologies'    (TT n) x = TT $ n { rootLabel = decoration { dImpliedAlignment = x } }
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

   addNode _ _ = error "addNode called on a TestingTree. Not implemented, don't call it!" -- Just don't call this!

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


data CharacterValueComparison
   = AllCharactersMatched
   | MismatchedCharacterValues [String]
   deriving (Eq, Show)

mismatches :: CharacterValueComparison -> [String]
mismatches AllCharactersMatched           = []
mismatches (MismatchedCharacterValues xs) = xs

simpleTreeCharacterDecorationEqualityAssertion :: Foldable t
               => Int                                -- ^ Root node reference
               -> String                             -- ^ Alphabet symbols
               -> (SimpleTree -> SimpleTree)         -- ^ Topology invariant tree transformation.
               -> (SimpleTree -> Vector DynamicChar) -- ^ Node accessing function
               -> t (Int, String, [String], [Int])   -- ^ (Node Reference, sequence of dynamic characters, expected values, child nodes)
               -> Assertion                      
simpleTreeCharacterDecorationEqualityAssertion rootRef symbols transformation accessor spec =
    assertMinimalFailure $ compareTree outputTree <$> valueTrees
  where
    assertMinimalFailure :: [CharacterValueComparison] -> Assertion
    assertMinimalFailure comparisons =
      case minimumBy (comparing length) $ mismatches <$> comparisons of
        [] ->  True @=? True
        ys -> assertFailure . (<> suffix) $ unlines ys
      where
        suffix = "In the transformed tree: \n" <> indentBlock (show outputTree)

    compareTree :: SimpleTree -> SimpleTree -> CharacterValueComparison
    compareTree actualValueTree expectedValueTree =
      case catMaybes $ checkTree' actualValueTree expectedValueTree of
        [] -> AllCharactersMatched
        es -> MismatchedCharacterValues es
      where
        checkTree' :: SimpleTree -> SimpleTree -> [Maybe String]
        checkTree' actualValueNode expectedValueNode
          | notEqualReference ||
            length xs /= length ys = [Just "The tree topology changed!"] 
          | actual    /= expected  = Just failureMessage : recursiveFailures
          | otherwise              = Nothing : recursiveFailures
          where
            recursiveFailures = concat $ zipWith checkTree' xs ys
            xs                = N.children actualValueNode   actualValueNode
            ys                = N.children expectedValueNode expectedValueNode
            expected          = EN.getEncoded expectedValueNode
            actual            =  accessor actualValueNode
            notEqualReference = not $ expectedValueNode `sameRef` actualValueNode
            nodeAlphabet      = suppliedAlphabet . rootLabel $ (\(TT x) -> x) actualValueNode
            failureMessage    = "For Node ( " <> show (nodeRef actualValueNode) <> " )\n" <>
                                (indentBlock . unlines)
                              [ "Expected value: " <> seqShow expected
                              , "Actual value  : " <> seqShow actual
                              ]
              where
                seqShow = indentLine . maybe "Empty sequence" (renderDynamicCharacter nodeAlphabet) . headMay 

    indentLine  = ("  " <>)
    indentBlock = unlines . fmap indentLine . lines

    -- Construct the tree to be transformed.
    inputTree   = createSimpleTree rootRef symbols . fmap (\(x,y,_,z) -> (x,y,z)) $ toList spec

    -- The result of the tree transformation.
    outputTree  = transformation inputTree

    -- A list of "comparison" trees, each tree in the list is a possible correct decoration after the tree transformation.
    valueTrees  = toValueTree <$> [0 .. valueTreeCount -1]

    valueTreeCount = length . fst . head $ otoList mapping

    -- Takes an index from the list of possible tree decorations and constructs a "comparison" tree.
    toValueTree j = TT . setRefIds $ unfoldTree buildExpectedTree rootRef 
      where
        buildExpectedTree :: Int -> (TestingDecoration, [Int])
        buildExpectedTree i = (def { dEncoded = encodedSequence }, otoList children)
          where
            encodedSequence
              | null (expectedChar !! j) = mempty
              | otherwise                =  pure . encodeStream alphabet . NE.fromList $ (\c -> [c]:|[]) <$> (expectedChar !! j)
            (expectedChar, children) = mapping ! i

    -- 
    alphabet = fromSymbols $ pure <$> symbols
--    mapping :: (Foldable a, Foldable c, Foldable v) => IntMap (v (c (a String)), IntSet)
    mapping = foldl' f mempty spec
      where
        f m (i, _, valChar, adjacency) = insertWith g i (valChar, IS.fromList adjacency) m
          where
            g (newSeq, lhs) (_, rhs) = (newSeq, lhs <> rhs)

