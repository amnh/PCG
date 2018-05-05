{-# ANN module "HLint: ignore Evaluate" #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.BitMatrix.Test
  ( testSuite
  ) where

import Control.DeepSeq
import Control.Exception
import Data.BitMatrix
import Data.BitVector.LittleEndian
import Data.Either
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Monoid ()
import Data.MonoTraversable
import Data.Semigroup
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck


newtype DependantFromRowsParameters
      = DependantFromRowsParameters
      { getParameters :: (Word, Word, [BitVector])
      } deriving (Eq, Show)


newtype FactoredBitVector
      = FactoredBitVector
      { getFactoredBitVector :: (Word, Word, BitVector)
      } deriving (Eq, Show)


instance Arbitrary DependantFromRowsParameters where

    arbitrary = do
        rowCount   <- getPositive <$> arbitrary
        colCount   <- getPositive <$> arbitrary
        let bvGen  =  fromBits    <$> vectorOf colCount (arbitrary :: Gen Bool)
        bitVectors <- vectorOf rowCount bvGen
        pure $ DependantFromRowsParameters (toEnum rowCount, toEnum colCount, bitVectors)


instance Arbitrary FactoredBitVector where

    arbitrary = do
        rowCount  <- getPositive <$> arbitrary
        colCount  <- getPositive <$> arbitrary
        bitVector <- fromBits    <$> vectorOf (colCount * rowCount) (arbitrary :: Gen Bool)
        pure $ FactoredBitVector (toEnum rowCount, toEnum colCount, bitVector)


testSuite :: TestTree
testSuite = testGroup "BitMatrix tests"
    [ monoFoldableProperties
    , monoFunctorProperties
    , monoTraversableProperties
    , orderingProperties
    , datastructureTests
    ]


monoFoldableProperties :: TestTree
monoFoldableProperties = testGroup "Properties of MonoFoldable"
    [ testProperty "ofoldr f z t === appEndo (ofoldMap (Endo . f) t ) z" testFoldrFoldMap
    , testProperty "ofoldl' f z t === appEndo (getDual (ofoldMap (Dual . Endo . flip f) t)) z" testFoldlFoldMap
    , testProperty "ofoldr f z === ofoldr f z . otoList" testFoldr
    , testProperty "ofoldl' f z === ofoldl' f z . otoList" testFoldl
    , testProperty "ofoldr1Ex f z === ofoldr1Ex f z . otoList" testFoldr1
    , testProperty "ofoldl1Ex' f z === ofoldl1Ex' f z . otoList" testFoldl1
    , testProperty "oall f === getAll . ofoldMap (All . f)" testAll
    , testProperty "oany f === getAny . ofoldMap (Any . f)" testAny
    , testProperty "olength === length . otoList" testLength
    , testProperty "onull === (0 ==) . olength" testNull
    , testProperty "headEx === getFirst . ofoldMap1Ex First" testHead
    , testProperty "lastEx === getLast . ofoldMap1Ex Last" testTail
    , testProperty "oelem e /== onotElem e" testInclusionConsistency
    ]
  where
    testFoldrFoldMap :: (Blind (BitVector -> Word -> Word), Word, BitMatrix) -> Property
    testFoldrFoldMap (Blind f, z, bm) =
        ofoldr f z bm === appEndo (ofoldMap (Endo . f) bm) z

    testFoldlFoldMap :: (Blind (Word -> BitVector -> Word), Word, BitMatrix) -> Property
    testFoldlFoldMap (Blind f, z, bm) =
        ofoldl' f z bm === appEndo (getDual (ofoldMap (Dual . Endo . flip f) bm)) z

    testFoldr :: (Blind (BitVector -> Word -> Word), Word, BitMatrix) -> Property
    testFoldr (Blind f, z, bm) =
        ofoldr f z bm === (ofoldr f z . otoList) bm
    
    testFoldl :: (Blind (Word -> BitVector -> Word), Word, BitMatrix) -> Property
    testFoldl (Blind f, z, bm) =
        ofoldl' f z bm === (ofoldl' f z . otoList) bm
    
    testFoldr1 :: (Blind (BitVector -> BitVector -> BitVector), BitMatrix) -> Property
    testFoldr1 (Blind f, bm) =
        (not . onull) bm  ==> ofoldr1Ex f bm === (ofoldr1Ex f . otoList) bm
    
    testFoldl1 :: (Blind (BitVector -> BitVector -> BitVector), BitMatrix) -> Property
    testFoldl1 (Blind f, bm) =
        (not . onull) bm  ==> ofoldl1Ex' f bm === (ofoldl1Ex' f . otoList) bm

    testAll :: (Blind (BitVector -> Bool), BitMatrix) -> Property
    testAll (Blind f, bm) =
        oall f bm === (getAll . ofoldMap (All . f)) bm

    testAny :: (Blind (BitVector -> Bool), BitMatrix) -> Property
    testAny (Blind f, bm) =
        oany f bm === (getAny . ofoldMap (Any . f)) bm

    testLength :: BitMatrix -> Property
    testLength bm =
        olength bm === (length . otoList) bm

    testNull :: BitMatrix -> Property
    testNull bm =
        onull bm === ((0 ==) . olength) bm

    testHead :: BitMatrix -> Property
    testHead bm =
        (not . onull) bm ==> headEx bm === (getFirst . ofoldMap1Ex First) bm
    
    testTail :: BitMatrix -> Property
    testTail bm =
        (not . onull) bm ==> lastEx bm === (getLast . ofoldMap1Ex Last) bm

    testInclusionConsistency :: (BitVector, BitMatrix) -> Property
    testInclusionConsistency (bv, bm) =
        oelem bv bm === (not . onotElem bv) bm


monoFunctorProperties :: TestTree
monoFunctorProperties = testGroup "Properites of a MonoFunctor"
    [ testProperty "omap id === id" omapId
    , testProperty "omap (f . g)  === omap f . omap g" omapComposition
    ]
  where
    omapId :: BitMatrix -> Property
    omapId bm = omap id bm === id bm

    omapComposition :: (Blind (BitVector -> BitVector), Blind (BitVector -> BitVector), BitMatrix) -> Property
    omapComposition (Blind f, Blind g, bm) =
         (omap f . omap g) bm `equalityWithExceptions` omap (f . g) bm


monoTraversableProperties :: TestTree
monoTraversableProperties = testGroup "Properties of MonoTraversable"
    [ testProperty "t . otraverse f === otraverse (t . f)" testNaturality
    , testProperty "otraverse Identity === Identity" testIdentity
    , testProperty "otraverse (Compose . fmap g . f) === Compose . fmap (otraverse g) . otraverse f" testComposition
    ]
  where
    testNaturality :: Blind (BitVector -> [BitVector]) -> BitMatrix -> Property
    testNaturality (Blind f) bm =
        (headMay . otraverse f) bm `equalityWithExceptions` otraverse (headMay . f) bm

    testIdentity :: BitMatrix -> Property
    testIdentity bm =
        otraverse Identity bm === Identity bm

    testComposition 
      :: Blind (BitVector -> Either Word BitVector)
      -> Blind (BitVector -> Maybe BitVector)
      -> BitMatrix
      -> Property
    testComposition (Blind f) (Blind g) bm =
        (Compose . fmap (otraverse g) . otraverse f) bm `equalityWithExceptions` otraverse (Compose . fmap g . f) bm
        

orderingProperties :: TestTree
orderingProperties = testGroup "Properties of an Ordering"
    [ testProperty "ordering preserves symetry"  symetry
    , testProperty "ordering is transitive (total)" transitivity
    ]
  where
    symetry :: (BitMatrix, BitMatrix) -> Bool
    symetry (lhs, rhs) =
        case (lhs `compare` rhs, rhs `compare` lhs) of
          (EQ, EQ) -> True
          (GT, LT) -> True
          (LT, GT) -> True
          _        -> False

    transitivity :: (BitMatrix, BitMatrix, BitMatrix) -> Property
    transitivity (a, b, c) = caseOne .||. caseTwo
      where
        caseOne = (a <= b && b <= c) ==> a <= c
        caseTwo = (a >= b && b >= c) ==> a >= c


datastructureTests :: TestTree
datastructureTests = testGroup "BitMatrix data structure tests"
    [ testRowsFromRows
    , testBitMatrix
    , testFromRows
    , testConsistentIndexing
    , testIsZeroMatrix
    , testRowsToList
    , testRowCountConsistency
    , testRowIndexConsistency
    , testExpandRows
    , testFactorRows
    , testExpandFactorIdentity
    ]


-- Just to make sure that rows . fromRows == id.
testRowsFromRows :: TestTree
testRowsFromRows = testProperty "rows . fromRows === id" f
  where
    f :: DependantFromRowsParameters -> Property
    f params = rows (fromRows bvList) === bvList
      where
        (_, _, bvList) = getParameters params


-- both sets of tests on generating functions rely on BitMatrix functions rows, numRows, numCols
testBitMatrix :: TestTree
testBitMatrix = testGroup "bitMatrix generating function"
    [ testValue
    , testWidth
    , testHeight
    , testRow
    ]
  where
    testValue = testProperty "Internal BitVector value is correct" f
      where
        f :: Positive Int -> Positive Int -> Property
        f rowCt colCt = testBM === controlBM
          where
            testBM    = mconcat $ rows (bitMatrix numChars alphLen $ const True)
            controlBM = fromNumber (alphLen * numChars) (2 ^ (alphLen * numChars) - 1 :: Integer)
            numChars  = toEnum $ getPositive rowCt
            alphLen   = toEnum $ getPositive colCt

    testWidth = testProperty "Number of columns is correct" f
      where
        f :: Positive Int -> Positive Int -> Property
        f rowCt colCt = expectedCols === numCols testBM
          where
            (_, expectedCols, testBM) = constructMatrixFromPositives rowCt colCt

    testHeight = testProperty "Number of rows is correct" f
      where
        f :: Positive Int -> Positive Int -> Property
        f rowCt colCt = expectedRows === numRows testBM
          where
            (expectedRows, _, testBM) = constructMatrixFromPositives rowCt colCt


constructMatrixFromPositives
  :: Positive Int         -- ^ Number of rows
  -> Positive Int         -- ^ Number of columns
  -> (Word, Word, BitMatrix) -- ^ Extracted Int values and resulting zero matrix
constructMatrixFromPositives rowCt colCt = (numRows', numCols', bitMatrix numRows' numCols' $ const False)
  where
    numRows' = toEnum $ getPositive rowCt
    numCols' = toEnum $ getPositive colCt


testFromRows :: TestTree
testFromRows = testGroup "Construction using fromRows"
    [ testValue
    , testWidth
    , testHeight
    ]
  where
    testValue = testProperty "Internal BitVector value is correct" f
    -- Relies on `rows` function.
      where
        f :: DependantFromRowsParameters -> Bool
        f params = testBM == controlBM
          where
            (_, _, bvs) = getParameters params
            testBM      = mconcat . rows $ fromRows bvs
            controlBM   = mconcat bvs
                        
    testWidth = testProperty "Number of columns is correct" f
      where
        f :: DependantFromRowsParameters -> Bool
        f params = colCt == numCols testBM
          where
            (_, colCt, bvs) = getParameters params
            testBM = fromRows bvs
                        
    testHeight = testProperty "Number of rows is correct" f
      where
        f :: DependantFromRowsParameters -> Bool
        f params = rowCt == numRows testBM
          where
            (rowCt, _, bvs) = getParameters params
            testBM = fromRows bvs


testRow :: TestTree
testRow = testProperty "row returns correct value" f
  where
    f :: DependantFromRowsParameters -> Bool
    f params = retVal 
      where 
        -- at each item in the list of bvs, test it againts what ought to be at that index, and accumulate
        (retVal, _)    = foldl' (\(bool, i) bv -> ((bv == row testBM i) && bool, i + 1)) (True, 0) bvList
        (_, _, bvList) = getParameters params
        testBM         = fromRows bvList



testConsistentIndexing :: TestTree
testConsistentIndexing = testProperty "Indexing and generation consistency" f
  where
    f :: Blind ((Word, Word) -> Bool) -> Gen Bool
    f (Blind g) = do
        rowCount    <- toEnum . getPositive <$> (arbitrary :: Gen (Positive Int))
        colCount    <- toEnum . getPositive <$> (arbitrary :: Gen (Positive Int))
        let bm      =  bitMatrix rowCount colCount g
        let indices =  [ (i,j) | i <- [0..rowCount-1], j <- [0..colCount-1] ]
        -- The generating function at a given index is the same as
        -- the bit tester at that index anfter bitMatrix generation.
        pure $ all (\x -> g x == bm `isSet` x) indices


testIsZeroMatrix :: TestTree
testIsZeroMatrix = testProperty "isZeroMatrix bm <===> ∀ i, not (bm `isSet` i)" f
  where
    -- The generating function always returns false if and only if
    -- the BitMatrix contains only zeros.
    f :: BitMatrix -> Bool
    f bm = all (not . isSet bm) indices == isZeroMatrix bm
      where
        indices = [ (i, j) | i <- [ 0 .. numRows bm - 1 ], j <- [ 0 .. numCols bm - 1 ] ]


testRowsToList :: TestTree
testRowsToList = testProperty "otoList === rows" f
  where
    f :: BitMatrix -> Property
    f bm = otoList bm === rows bm


testRowCountConsistency :: TestTree
testRowCountConsistency = testProperty "numRows === length . rows" f
  where
    f :: BitMatrix -> Property
    f bm = numRows bm === (toEnum . length . rows) bm


testRowIndexConsistency :: TestTree
testRowIndexConsistency = testProperty "∀ i, (`row` i) === (! i) . rows" f
  where
    f :: BitMatrix -> Property
    f bm = conjoin $ g <$> [ 0 .. numRows bm - 1 ]
      where
        g i = bm `row` i === rows bm !! fromEnum i


testExpandRows :: TestTree
testExpandRows = testProperty "toBits . expandRows === fmap (isSet bm)" f
  where
    f :: BitMatrix -> Property
    f bm = (toBits . expandRows) bm === (isSet bm <$> indices)
      where
        indices = [ (i, j) | i <- [ 0 .. numRows bm - 1 ], j <- [ 0 .. numCols bm - 1 ] ]


testFactorRows :: TestTree
testFactorRows = testProperty "toBits === fmap (isSet bm) . factorRows n" f
  where
    f :: FactoredBitVector -> Property
    f input = toBits bv === (isSet bm <$> indices)
      where
        bm = factorRows colCount bv
        indices = [ (i, j) | i <- [ 0 .. numRows bm - 1 ], j <- [ 0 .. numCols bm - 1 ] ]
        (_, colCount, bv) = getFactoredBitVector input

          
testExpandFactorIdentity :: TestTree
testExpandFactorIdentity = testProperty "factorRows numCols . expandRows === id" f
  where
    f :: BitMatrix -> Property
    f bm = factorRows (numCols bm) (expandRows bm) === bm


-- |
-- Should either pass the test or throw an exception.
equalityWithExceptions :: (Eq a, NFData a, Show a) => a -> a -> Property
equalityWithExceptions x y = monadicIO $ do
    lhs <- supressException x
    rhs <- supressException y
    pure $ case lhs of
            Left  _ -> anyException
            Right a ->
              case rhs of
                Left  _ -> anyException
                Right b -> a === b
  where
    supressException :: NFData a => a -> PropertyM IO (Either SomeException a)
    supressException = run . try . evaluate . force

    anyException :: Property
    anyException = True === True
