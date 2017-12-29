{-# LANGUAGE FlexibleInstances #-}

module Data.BitMatrix.Test
  ( testSuite
  ) where

import Data.Bits
import Data.BitMatrix
import Data.BitVector.LittleEndian
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Monoid ()
import Data.MonoTraversable
import Data.Semigroup
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))


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
    [ bitsTests
    , monoFunctorProperties
    , monoFoldableProperties
    , monoTraversableProperties
    , orderingProperties
    , datastructureTests
    ]

bitsTests :: TestTree
bitsTests = testGroup "Bits instance properties"
    [ testProperty "∀ n ≥ 0, clearBit zeroBits n === zeroBits" zeroBitsAndClearBit
    , testProperty "∀ n ≥ 0, setBit   zeroBits n === bit n" zeroBitsAndSetBit
    , testProperty "∀ n ≥ 0, testBit  zeroBits n === False" zeroBitsAndTestBit
    , testCase     "         popCount zeroBits   === 0" zeroBitsAndPopCount
    , testProperty "complement === omap not" complementOmapNot
    , testProperty "(`setBit` n) === (.|. bit n)" setBitDefinition
    , testProperty "(`clearBit` n) === (.&. complement (bit n))" clearBitDefinition
    , testProperty "(`complementBit` n) === (`xor` bit n)" complementBitDefinition
    , testProperty "(`testBit` n) . (`setBit` n)" testBitAndSetBit
    , testProperty "not  . (`testBit` n) . (`clearBit` n)" testBitAndClearBit
    ]
  where
    zeroBitsAndClearBit :: NonNegative Int -> Property
    zeroBitsAndClearBit (NonNegative n) =
        clearBit (zeroBits :: BitVector) n === zeroBits

    zeroBitsAndSetBit :: NonNegative Int -> Property
    zeroBitsAndSetBit (NonNegative n) =
        setBit   (zeroBits :: BitVector) n === bit n
      
    zeroBitsAndTestBit :: NonNegative Int -> Property
    zeroBitsAndTestBit (NonNegative n) =
        testBit  (zeroBits :: BitVector) n === False
      
    zeroBitsAndPopCount :: Assertion
    zeroBitsAndPopCount =
        popCount (zeroBits :: BitVector) @?= 0

    complementOmapNot :: BitVector -> Property
    complementOmapNot bv =
        complement bv === omap not bv

    setBitDefinition :: (NonNegative Int, BitVector) -> Property
    setBitDefinition (NonNegative n, bv) =
        bv `setBit` n === bv .|. bit n

    clearBitDefinition :: (NonNegative Int, BitVector) -> Property
    clearBitDefinition (NonNegative n, bv) =
        n < (fromEnum . dimension) bv ==>
          (bv `clearBit` n === bv .&. complement  (zed .|. bit n))
      where
        zed = bitvector (dimension bv) (0 :: Integer)

    complementBitDefinition :: (NonNegative Int, BitVector) -> Property
    complementBitDefinition (NonNegative n, bv) =
        bv `complementBit` n === bv `xor` bit n

    testBitAndSetBit :: (NonNegative Int, BitVector) -> Bool
    testBitAndSetBit (NonNegative n, bv) =
        ((`testBit` n) . (`setBit` n)) bv

    testBitAndClearBit :: (NonNegative Int, BitVector) -> Bool
    testBitAndClearBit (NonNegative n, bv) =
        (not  . (`testBit` n) . (`clearBit` n)) bv
    

monoFunctorProperties :: TestTree
monoFunctorProperties = testGroup "Properites of a MonoFunctor"
    [ testProperty "omap id === id" omapId
    , testProperty "omap (f . g)  === omap f . omap g" omapComposition
    ]
  where
    omapId :: BitVector -> Property
    omapId bv = omap id bv === id bv
    
    omapComposition :: (Blind (Bool -> Bool), Blind (Bool -> Bool), BitVector) -> Property
    omapComposition (Blind f, Blind g, bv) = omap (f . g) bv ===  (omap f . omap g) bv


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
    testFoldrFoldMap :: (Blind (Bool -> Word -> Word), Word, BitVector) -> Property
    testFoldrFoldMap (Blind f, z, bv) =
        ofoldr f z bv === appEndo (ofoldMap (Endo . f) bv) z

    testFoldlFoldMap :: (Blind (Word -> Bool -> Word), Word, BitVector) -> Property
    testFoldlFoldMap (Blind f, z, bv) =
        ofoldl' f z bv === appEndo (getDual (ofoldMap (Dual . Endo . flip f) bv)) z

    testFoldr :: (Blind (Bool -> Word -> Word), Word, BitVector) -> Property
    testFoldr (Blind f, z, bv) =
        ofoldr f z bv === (ofoldr f z . otoList) bv
    
    testFoldl :: (Blind (Word -> Bool -> Word), Word, BitVector) -> Property
    testFoldl (Blind f, z, bv) =
        ofoldl' f z bv === (ofoldl' f z . otoList) bv
    
    testFoldr1 :: (Blind (Bool -> Bool -> Bool), BitVector) -> Property
    testFoldr1 (Blind f, bv) =
        (not . onull) bv  ==> ofoldr1Ex f bv === (ofoldr1Ex f . otoList) bv
    
    testFoldl1 :: (Blind (Bool -> Bool -> Bool), BitVector) -> Property
    testFoldl1 (Blind f, bv) =
        (not . onull) bv  ==> ofoldl1Ex' f bv === (ofoldl1Ex' f . otoList) bv

    testAll :: (Blind (Bool -> Bool), BitVector) -> Property
    testAll (Blind f, bv) =
        oall f bv === (getAll . ofoldMap (All . f)) bv

    testAny :: (Blind (Bool -> Bool), BitVector) -> Property
    testAny (Blind f, bv) =
        oany f bv === (getAny . ofoldMap (Any . f)) bv

    testLength :: BitVector -> Property
    testLength bv =
        olength bv === (length . otoList) bv

    testNull :: BitVector -> Property
    testNull bv =
        onull bv === ((0 ==) . olength) bv

    testHead :: BitVector -> Property
    testHead bv =
        (not . onull) bv ==> headEx bv === (getFirst . ofoldMap1Ex First) bv
    
    testTail :: BitVector -> Property
    testTail bv =
        (not . onull) bv ==> lastEx bv === (getLast . ofoldMap1Ex Last) bv

    testInclusionConsistency :: (Bool, BitVector) -> Property
    testInclusionConsistency (e, bv) =
        oelem e bv === (not . onotElem e) bv


monoTraversableProperties :: TestTree
monoTraversableProperties = testGroup "Properties of MonoTraversable"
    [ testProperty "t . otraverse f === otraverse (t . f)" testNaturality
    , testProperty "otraverse Identity === Identity" testIdentity
    , testProperty "otraverse (Compose . fmap g . f) === Compose . fmap (otraverse g) . otraverse f" testComposition
    ]
  where
    testNaturality :: (Blind (Bool -> [Bool]), BitVector) -> Property
    testNaturality (Blind f, bv) =
        (headMay . otraverse f) bv === otraverse (headMay . f) bv

    testIdentity :: BitVector -> Property
    testIdentity bv =
        otraverse Identity bv === Identity bv

    testComposition :: (Blind (Bool -> Either Word Bool), Blind (Bool -> Maybe Bool), BitVector) -> Property
    testComposition (Blind f, Blind g, bv) =
        otraverse (Compose . fmap g . f) bv === (Compose . fmap (otraverse g) . otraverse f) bv


orderingProperties :: TestTree
orderingProperties = testGroup "Properties of an Ordering"
    [ testProperty "ordering preserves symetry"  symetry
    , testProperty "ordering is transitive (total)" transitivity
    ]
  where
    symetry :: (BitVector, BitVector) -> Bool
    symetry (lhs, rhs) =
        case (lhs `compare` rhs, rhs `compare` lhs) of
          (EQ, EQ) -> True
          (GT, LT) -> True
          (LT, GT) -> True
          _        -> False

    transitivity :: (BitVector, BitVector, BitVector) -> Property
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
            controlBM = bitvector (alphLen * numChars) (2 ^ (alphLen * numChars) - 1 :: Integer)
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
        g i = bm `row` i === rows bm !! (fromEnum i)


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
