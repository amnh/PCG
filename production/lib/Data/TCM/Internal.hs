-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TCM.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE Strict       #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TCM.Internal where

import           Control.Arrow                 ((***))
import           Data.Foldable
import           Data.List                     (transpose)
import           Data.List.Utility             (equalityOf, occurances)
import           Data.Map                      (delete, findMax, keys)
import qualified Data.Map             as Map   (fromList)
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Ratio
import           Data.Vector.Unboxed           (Vector)
import qualified Data.Vector.Unboxed  as V
import           Data.Word
import           Prelude              hiding   (lookup)
import           Test.QuickCheck


-- | A data structure for storing a two dimensional, square array of dimensionality
--   greater that or equal to two with positive cost values at the array indices.
--   Values are stored in an unboxed structure for cache efficiency.
--
--  A 'TCM' can be constructed by calling one of the following functions:
--
--    * 'fromList'
--
--    * 'fromCols'
--
--    * 'fromRows'
--
--    * 'generate'
--
--  Attempts to construct an empty or singleton 'TCM' through the above constructors
--  will result in a runtime exception.
data TCM
   = TCM Int (Vector Word32)
   deriving (Eq)


-- |
-- The monomorphic element of a TCM representing the transition cost between to state symbols.
type instance Element TCM = Word32


-- |
-- There is a heirachichal nature to a TCM's possible structure:
--
-- @
--       NonSymmetric
--
--           |
--
--        Symmetric
--
--           |
-- 
--         Metric
--
--         /    \\
--
-- UltraMetric  Additive
--
--      |
--
-- NonAdditive
--
-- @
--
-- Each TCM structure has certain properties that must hold and allows for space
-- & time optimizations.
--
--   * Non-Symmetric: No structure expoitable for optimizations.
--
--   * Symmetric: Allows for half the space allocation.
--
--       * /σ(i,j) = σ(j,i)/
--
--   * Metric: Allows for half the space allocation & runtime optimizations
--
--       * /σ(i,j) = 0 iff i = j/
--
--       * /σ(i,j) = σ(j,i)/
--
--       * /σ(i,k) ≤ σ(i,j) + σ(j,k)/
--
--  * Ultra-Metric: Allows for half the space allocation & runtime optimizations
--
--       * /σ(i,j) = 0 iff i = j/
--
--       * /σ(i,j) = σ(j,i)/
--
--       * /σ(i,k) ≤ max { σ(i,j),  σ(j,k) }/
--
--   * Non-Additive: Allows for /no/ space allocation & runtime optimizations
--
--       * /σ(i,j) = 0 iff i = j/
--
--       * /σ(i,j) = 1 iff i ≠ j/
--
--   * Additive: Allows for /no/ space allocation & runtime optimizations
--
--       * /σ(i,j) = max(i,j) - min(i,j)/
--
data TCMStructure
   = NonSymmetric
   | Symmetric
   | Metric
   | UltraMetric
   | Additive
   | NonAdditive
   deriving (Eq, Show)


-- |
-- The result of a call to 'diagnoseTcm'.
data TCMDiagnosis
   = TCMDiagnosis
   { factoredWeight :: Int -- ^ The multiplicative constant factor of a 'TCM'. Minimum value of the multiplicative identiy /one/.
   , factoredTcm    :: TCM -- ^ The new 'TCM' with each value divided by the 'factoredWeight' .
   , tcmStructure   :: TCMStructure -- ^ The most restrictive present in the 'factoredTcm'.
   } deriving (Show)


-- | Performs a element-wise monomporphic map over the 'TCM'.
instance MonoFunctor TCM where

    omap f (TCM n v) = TCM n $ V.map f v


-- | Performs a row-major monomporphic fold over the 'TCM'.
instance MonoFoldable TCM where

    -- | Map each element of a structure to a 'Monoid' and combine the results.
    {-# INLINE ofoldMap #-}
    ofoldMap f = V.foldr (mappend . f) mempty . vec

    -- | Right-associative fold of a structure.
    {-# INLINE ofoldr #-}
    ofoldr f e = V.foldr f e . vec

    -- | Strict left-associative fold of a structure.
    {-# INLINE ofoldl' #-}
    ofoldl' f e = V.foldl' f e . vec

    -- | Right-associative fold of a monomorphic container with no base element.
    --
    -- Note: this is a partial function. On an empty 'MonoFoldable', it will
    -- throw an exception.
    --
    -- /See 'Data.MinLen.ofoldr1Ex' from "Data.MinLen" for a total version of this function./
    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex f = V.foldr1 f . vec

    -- | Strict left-associative fold of a monomorphic container with no base
    -- element.
    --
    -- Note: this is a partial function. On an empty 'MonoFoldable', it will
    -- throw an exception.
    --
    -- /See 'Data.MinLen.ofoldl1Ex'' from "Data.MinLen" for a total version of this function./
    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex' f = V.foldl1' f . vec

    {-# INLINE otoList #-}
    otoList = V.toList . vec

    {-# INLINE onull #-}
    onull = const False

    {-# INLINE olength #-}
    olength = V.length .vec


-- | Performs a row-major monomporphic traversal over ther 'TCM'.
instance MonoTraversable TCM where

    -- | Map each element of a monomorphic container to an action,
    -- evaluate these actions in row-major order and collect the results.
    {-# INLINE otraverse #-}
    otraverse f (TCM n v) = fmap (TCM n . V.fromList) . traverse f $ V.toList v

    -- | Map each element of a monomorphic container to a monadic action,
    -- evaluate these actions from left to right, and
    -- collect the results.
    {-# INLINE omapM #-}
    omapM = otraverse


-- | Resulting TCMs will have at a dimension between 2 and 25.
instance Arbitrary TCM where

    arbitrary = do 
        dimension  <- (arbitrary :: Gen Int) `suchThat` (\x -> 2 <= x && x <= 25) 
        dataVector <- V.fromList <$> vectorOf (dimension * dimension) arbitrary
        pure $ TCM dimension dataVector


-- |
-- A pretty printed custom show instance for 'TCM'.
instance Show TCM where

    show tcm = headerLine <> matrixLines
      where
        renderRow i = ("  "<>) . unwords $ renderValue <$> [ tcm ! (i,j) | j <- rangeValues ]
        matrixLines = unlines $ renderRow   <$> rangeValues
        rangeValues = [0 .. size tcm - 1] 
        headerLine  = '\n' : unwords [ "TCM:", show $ size tcm, "x", show $ size tcm, "\n"]
        maxValue    = maximumEx tcm
        padSpacing  = length $ show maxValue
        renderValue x = pad <> shown
          where
            shown = show x
            pad   = (padSpacing - length shown) `replicate` ' '
        
        
-- | /O(1)/ Indexing without bounds checking.
{-# INLINE (!) #-}
(!) :: Enum i => TCM -> (i, i) -> Word32
(!) (TCM n v) (i,j) = v `V.unsafeIndex` (fromEnum i * n + fromEnum j)


-- | /O(1)/ Safe indexing.
{-# INLINE (!?) #-}
(!?) :: Enum i => TCM -> (i, i) -> Maybe Word32
(!?) (TCM n v) (i,j) = v V.!? (fromEnum i * n + fromEnum j)


-- | /O(1)/
--
--   The number of rows and columns in the 'TCM'.
{-# INLINE size #-}
size :: TCM -> Int
size (TCM x _) = x


-- | /O(n*n)/
--
--   Construct a 'TCM' from a list of elements in row major order.
--
-- ==== __Examples__
--
-- >>> fromList [1..9]
-- TCM: 3 x 3
--   1 2 3
--   4 5 6
--   7 8 9
--
-- >>> fromList []
-- *** Exception: fromList: An empty structure was supplied. Cannot construct an empty TCM!
--
-- >>> fromList [42]
-- *** Exception: fromList: A singleton structure was supplied. Cannot construct a TCM with dimension of 1, must have dimension of 2 or greater.
--
-- >>> fromList [1..12]
-- *** Exception: fromList: The number of element (12) is not a square number. Cannot construct an non-square TCM! The number of elements (12) lies between the valid square numbers (9) and (16).
--
fromList :: (Foldable t, Real a) => t a -> (Rational, TCM)
fromList xs
  | null xs       = error "fromList: An empty structure was supplied. Cannot construct an empty TCM!"
  | notSquareList = error notSquareErrorMsg
  | dimension < 2 = error "fromList: A singleton structure was supplied. Cannot construct a TCM with dimension of 1, must have dimension of 2 or greater."
  | otherwise     = fromListUnsafe xs
  where
--    resultVector  = V.fromList $ coerce <$> toList xs
    len           = length xs
    dimension     = floor $ sqrt (fromIntegral len :: Double)
    notSquareList = square dimension /= len
    square x      = x*x
    notSquareErrorMsg = mconcat [ "fromList: The number of element ("
                                , show len
                                ,") is not a square number. "
                                , "Cannot construct an non-square TCM! "
                                , "The number of elements ("
                                , show len
                                , ") lies between the valid square numbers ("
                                , show $ square dimension
                                , ") and ("
                                , show $ square (dimension + 1)
                                , ")."
                                ]


-- | /O(n*n)/
--
--   Construct a 'TCM' from a list of columns.
--
-- ==== __Examples__
--
-- >>> fromCols [[1,2,3],[4,5,6],[7,8,9]]
-- TCM: 3 x 3
--   1 4 7
--   2 5 8
--   3 6 9
--
fromCols :: (Foldable t, Foldable t', Real a) => t (t' a) -> (Rational, TCM)
fromCols xs
  | null xs          = error "fromCols: An empty structure was supplied. Cannot construct an empty TCM!"
  | hasJaggedCols    = error jaggedColsErrorMsg
  | width /= height  = error notSquareErrorMsg
  | height < 2       = error "fromCols: A singleton structure was supplied. Cannot construct a TCM with dimension of 1, must have dimension of 2 or greater."
  | otherwise        = fromListUnsafe . mconcat . transpose $ toList <$> toList xs 
  where
    width            = length xs
    height           = length . head $ toList xs
    hasJaggedCols    = not . equalityOf length $ toList xs 

    jaggedColsErrorMsg = mconcat 
                       [ "fromCols: All the columns did not have the same height! "
                       , "Expected modal height of ("
                       , show mode
                       , ") but found other heights of "
                       , show otherLengths
                       ]
      where
        (mode, otherLengths) = modeAndOutlierLengths xs
        
    notSquareErrorMsg = mconcat [ "fromRows: The number of rows ("

                                , show height
                                ,") did not match the number of columns ("
                                , show width
                                , ")!"
                                ]


-- | /O(n*n)/
--
--   Construct a 'TCM' from a list of rows.
--
-- ==== __Examples__
--
-- >>> fromRows [[1,2,3],[4,5,6],[7,8,9]]
-- TCM: 3 x 3
--   1 2 3
--   4 5 6
--   7 8 9
--
fromRows :: (Foldable t, Foldable t', Real a) => t (t' a) -> (Rational, TCM)
fromRows xs
  | null xs          = error "fromRows: An empty structure was supplied. Cannot construct an empty TCM!"
  | hasJaggedRows    = error jaggedRowsErrorMsg
  | width /= height  = error notSquareErrorMsg
  | height < 2       = error "fromRows: A singleton structure was supplied. Cannot construct a TCM with dimension of 1, must have dimension of 2 or greater."
  | otherwise        = fromListUnsafe . foldMap toList $ toList xs 
  where
    height           = length xs
    width            = length . head $  toList xs
    hasJaggedRows    = not $ equalityOf length xs

    jaggedRowsErrorMsg = mconcat 
                       [ "fromRows: All the rows did not have the same width! "
                       , "Expected modal width of ("
                       , show mode
                       , ") but found other widths of "
                       , show otherLengths
                       ]
      where
        (mode, otherLengths) = modeAndOutlierLengths xs
        
    notSquareErrorMsg = mconcat [ "fromRows: The number of rows ("
                                , show height
                                ,") did not match the number of columns ("
                                , show width
                                , ")!"
                                ]


-- |
-- Determines the mode length and the other lengths of a nested foldable structure.
modeAndOutlierLengths :: (Foldable t, Foldable t') => t (t' a) -> (Int, [Int])
modeAndOutlierLengths xs = (mode, otherLengths)
  where
    occuranceMap = Map.fromList . occurances $ length <$> toList xs
    (mode,_)     = findMax occuranceMap
    otherLengths = keys  $ mode `delete` occuranceMap


-- | /O(n*n)/
--
--   A generating function for a 'TCM'. Efficiently constructs a
--   'TCM' of the specified dimensions with each value defined by the result
--   of the supplied function.
--
-- ==== __Examples__
--
-- >>> generate 5 $ const 5
-- TCM: 5 x 5
--   5 5 5 5 5
--   5 5 5 5 5
--   5 5 5 5 5
--   5 5 5 5 5
--   5 5 5 5 5
--
-- >>> generate 4 $ \(i,j) -> abs (i - j)
-- TCM: 4 x 4
--   0 1 2 3 4
--   1 0 1 2 3
--   2 1 0 1 2
--   3 2 1 0 1
--   4 3 2 1 0
--
-- >>> generate 8 $ \(i,j) -> if i == j || i + j == 6 then 0 else 1
-- TCM: 8 x 8
--   0 1 1 1 0 1 1 1
--   1 0 1 0 1 1 1 1
--   1 1 0 1 1 1 1 1
--   1 0 1 0 1 1 1 1
--   0 1 1 1 0 1 1 1
--   1 1 1 1 1 0 1 1
--   1 1 1 1 1 1 0 1
--   1 1 1 1 1 1 1 0
--
generate :: ( Enum i
            , Integral a
            )
         => Int          -- ^ Number of rows & columns in the TCM.
         -> ((i,i) -> a) -- ^ Function to determine the value of a given index.
         -> TCM
generate n f
  | n <  0    = error negativeErrorMessage
  | n == 0    = error nullErrorMessage
  | n == 1    = error singletonErrorMessage
  | otherwise = TCM n resultVector
  where
    resultVector = V.generate (n*n) g
      where
        g i = coerce . f . (toEnum *** toEnum) $ (i `divMod` n)
    negativeErrorMessage = mconcat
      [ "The call to 'generate ", show n, " f' is malformed, "
      , "the dimension (", show n, ") is a negative number. "
      , "Cannot construct a TCM with a negative dimension!"
      ]
    nullErrorMessage = mconcat
      [ "The call to 'generate 0 f' is malformed, "
      , "the dimension is zero. "
      , "Cannot construct an empty TCM with a nullary dimension!"
      ]
    singletonErrorMessage = mconcat
      [ "The call to 'generate 1 f' is malformed, "
      , "the dimension is one. "
      , "Cannot construct a singlton TCM with a dimension of one!"
      ]


-- |
-- Performs a diagnosis of a TCM factoring out any multiplicative factor as a
-- constant weighting and reporting the most restricitive structure present in
-- the TCM.
diagnoseTcm :: TCM -> TCMDiagnosis
diagnoseTcm tcm
  | isNonAdditive  tcm' = diagnosis  NonAdditive
  | isAdditive     tcm' = diagnosis     Additive
  | isUltraMetric  tcm' = diagnosis  UltraMetric
  | isMetric       tcm' = diagnosis       Metric
  | isSymmetric    tcm' = diagnosis    Symmetric
  | otherwise           = diagnosis NonSymmetric
  where
    (weight, tcm') = factorTCM tcm
    diagnosis = TCMDiagnosis weight tcm'


-- Un-exported Functionality
--------------------------------------------------------------------------------


-- |
-- Precondition of the list having a square number of elements was already
-- checked in the function wrapping calls to this method.
--
-- This method take a list of values coercable to 'Rational' values via the
-- 'Real' type-class and produces an integral valued TCM with a rational weight.
fromListUnsafe :: (Foldable t, Real a) => t a -> (Rational, TCM)
fromListUnsafe xs
  | not $ null negativeValues = error $ "The following negative values were found in the TCM: " <> show negativeValues
  | not $ null overflowValues = error $ "The following values are either too small or two large for the TCM's 32-bit precision: " <> show overflowValues
  | otherwise                 = ( 1 % coefficient, TCM dimension coercedVector)
  where
    dimension         = floor $ sqrt (fromIntegral (length xs) :: Double)
    coercedVector     = V.fromList $ toEnum . fromEnum <$> prospectiveValues
    negativeValues    = filter (< 0) rationalValues
    overflowValues    = fmap fst . filter (\(_,y) -> y > toRational (maxBound :: Word32)) $ zip rationalValues prospectiveValues
    prospectiveValues = ((coefficient % 1) *) <$> rationalValues
    coefficient       = foldl1 lcm $ abs . denominator <$> rationalValues
    rationalValues    = toRational <$> toList  xs


-- |
-- Determines if a constant positve multiplicative factor can be extracted from
-- every non-zero value of the TCM. Returns the multiplicive factor and the 'TCM'
-- with reduced values. The least factor that can be found is the multipliative
-- identiy /one/ when all values in the 'TCM' are reletively prime.
factorTCM :: TCM -> (Int, TCM)
factorTCM tcm
  | factor <= 1 = (x,                       tcm)
  | otherwise   = (x, (`div` factor) `omap` tcm)
  where
    factor = ofoldl1Ex' gcd tcm
    x = fromEnum factor


-- |
-- Determines if the TCM has an additive structure.
--
-- /Assumes/ the 'TCM' has already been factored with 'factorTCM'.
isAdditive :: TCM -> Bool
isAdditive tcm = all isAdditiveIndex [(i,j) | i <- range, j <- range ]
  where
    isAdditiveIndex (i,j) = tcm ! (i,j) == toEnum (max i j - min i j)
    range = [0 .. size tcm - 1]


-- |
-- Determines if the 'TCM' has an non-additive structure.
-- This is also colloquially reffered to as as unordered characters as originally
-- named by Fitch.
--
-- /Assumes/ the 'TCM' has already been factored with 'factorTCM'.
isNonAdditive :: TCM -> Bool
isNonAdditive tcm = all isNonAdditiveIndex [(i,j) | i <- range, j <- range ]
  where
    isNonAdditiveIndex (i,j) =  (i == j && tcm ! (i,j) == 0)
                             || (i /= j && tcm ! (i,j) == 1)
    range = [0 .. size tcm - 1]


-- |
-- Determines if the 'TCM' has a metric structure.
--
-- The properties of a metric space much hold for the 'TCM':
--
-- * /σ(i,j) = 0 iff i = j/
--
-- * /σ(i,j) = σ(j,i)/
--
-- * /σ(i,k) ≤ σ(i,j) + σ(j,k)/
isMetric :: TCM -> Bool
isMetric tcm = conditions `allSatisfiedBy` tcm
  where
    conditions =
        [ zeroDiagonalOnly
        , isSymmetric
        , triangleInequality
        ]
    triangleInequality x = all triangleInequalityIndex [(i,k,j) | i <- range, j <- range, k <- range, i < j, j < k ]
      where
        triangleInequalityIndex (i,j,k) = x ! (i,k) <= x ! (i,j) + x ! (j,k)
        range = [0 .. size tcm - 1]


-- |
-- Determines if the 'TCM' has a symmetric structure.
isSymmetric :: TCM -> Bool
isSymmetric tcm = all isSymmetricIndex [(i,j) | i <- range, j <- range, i <= j ]
  where
    isSymmetricIndex (i,j) = tcm ! (i,j) == tcm ! (j,i)
    range = [0 .. size tcm - 1]


-- |
-- Determines if the 'TCM' has an ultrametric structure.
--
-- The properties of an ultrametric space much hold for the 'TCM':
--
-- * /σ(i,j) = 0 iff i = j/
--
-- * /σ(i,j) = σ(j,i)/
--
-- * /σ(i,k) ≤ max { σ(i,j),  σ(j,k) }/
isUltraMetric :: TCM -> Bool
isUltraMetric tcm = conditions `allSatisfiedBy` tcm
  where
    conditions = 
        [ zeroDiagonalOnly      
        , isSymmetric           
        , ultraMetricInequality
        ]
    ultraMetricInequality x = all ultraMetricInequalityIndex [(i,k,j) | i <- range, j <- range, k <- range, i < j, j < k ]
      where
        ultraMetricInequalityIndex (i,j,k) = x ! (i,k) <= max (x ! (i,j)) (x ! (j,k))
        range = [0 .. size tcm - 1]


-- |
-- An internal helper function used in both 'isMetric' & 'isUltraMetric' exported functions.
zeroDiagonalOnly :: TCM -> Bool
zeroDiagonalOnly tcm = all zeroDiagonalIndex [ (i,j) | i <- range, j <- range ]
  where
    range = [0 .. size tcm - 1]
    zeroDiagonalIndex (i,j)
      | i == j    = value == 0
      | otherwise = value /= 0
      where
         value = tcm ! (i,j)


-- |
-- Takes a "list" of conditions to be satisfied and a element upon which to query
-- And returns whether *all* the conditions were satified.
allSatisfiedBy :: Foldable t => t (a -> Bool) -> a -> Bool
allSatisfiedBy conditions element = foldl' (&&) True $ toList conditions <*> pure element


-- |
-- Deconstructs the 'TCM' to expose the underlying unboxed 'Vector'.
{-# INLINE vec #-}
vec :: TCM -> Vector Word32
vec (TCM _ v) = v



-- |
-- Takes an 'Integral' value and converts it to an unsigned unboxable value.
{-# INLINE coerce #-}
coerce :: Integral a => a -> Word32
coerce = toEnum . fromEnum . toInteger



{-
-- Modified greeatest common divisor algorithm applied to rational numbers.
gcd' :: Rational -> Rational -> Maybe Rational
gcd' x y 
  | result < 1 = Nothing
  | otherwise  = Just result
  where
    result = gcd'' (abs x) (abs y) 
    gcd'' a 0  = a
    gcd'' a b
      | a < b     = gcd'' b  a
      | otherwise = gcd'' b (a `op` b)
    m `op` n = r * n
      where
       q = m / n
       p = numerator q `rem` denominator q
       r = p % denominator q

-- This allows for jagged "matries"
matrixGCD :: (Foldable t, Foldable t', Real a) => t (t' a) -> Maybe Rational
matrixGCD structure =
  case rowGCD <$> toList structure of
    [] -> Nothing
    xs -> foldl1 coalesce xs
  where
    rowGCD = streamProcessRow . fmap toRational . toList
      where
        streamProcessRow []       = Nothing
        streamProcessRow [x]      = Just x
        streamProcessRow [x,y]    = gcd' x y
        streamProcessRow (x:y:xs) = streamProcessRow' (gcd' x y) (y:xs)
        
        streamProcessRow' z [x,y]    = coalesce z $ gcd' x y
        streamProcessRow' z (x:y:xs) = streamProcessRow' acc (y:xs)
          where
            acc = coalesce z $ gcd' x y

    coalesce        _  Nothing = Nothing
    coalesce  Nothing        _ = Nothing
    coalesce (Just m) (Just n) = gcd' m n
       -}
