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

{-# LANGUAGE Strict, TypeFamilies #-}

module Data.TCM.Internal where

import           Data.Foldable
import           Data.List.Utility             (equalityOf, occurances)
import           Data.Map                      (delete, findMax, keys)
import qualified Data.Map             as Map   (fromList)
import           Data.Monoid
import           Data.MonoTraversable
import qualified Data.Vector          as Boxed
import           Data.Vector.Unboxed           (Vector)
import qualified Data.Vector.Unboxed  as V
import           Data.Word
import           Prelude              hiding   (lookup)
import           Test.QuickCheck


-- | A data structure for storing a two dimensional array of positive cost values.
--   Values are stored in an unboxed structure for cache efficiency.
--
data TCM
   = TCM Int (Vector Word32)
   deriving (Eq)

type instance Element TCM = Word32

-- | Performs a element-wise monomporphic map over ther 'TCM'.
instance MonoFunctor TCM where
    omap f (TCM n v) = TCM n $ V.map f v

-- | Performs a row-major monomporphic fold over ther 'TCM'.
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
  ofoldl1Ex' f = V.foldl1 f . vec

  {-# INLINE otoList #-}
  otoList = V.toList . vec

  {-# INLINE onull #-}
  onull = const False

  {-# INLINE olength #-}
  olength = V.length .vec


-- | Performs a row-wise monomporphic traversal over ther 'TCM'.
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


instance Show TCM where
    show tcm = headerLine <> matrixLines
      where
        renderRow i = ("  "<>) . unwords $ renderValue <$> [ tcm ! (i,j) | j <- rangeValues ]
        matrixLines = unlines $ renderRow   <$> rangeValues
        rangeValues = [0 .. (size tcm) - 1] 
        headerLine  = '\n' : unwords [ "TCM:", show $ size tcm, "x", show $ size tcm, "\n"]
        maxValue    = maximumEx tcm
        padSpacing  = length $ show maxValue
        renderValue x = pad <> shown
          where
            shown = show x
            pad   = (padSpacing - length shown) `replicate` ' '
        
        



-- | /O(1)/ Indexing without bounds checking.
{-# INLINE (!) #-}
(!) :: TCM -> (Int, Int) -> Word32
(!) (TCM n v) (i,j) = v `V.unsafeIndex` (i * n + j)


-- | /O(1)/ Safe indexing.
{-# INLINE (!?) #-}
(!?) :: TCM -> (Int, Int) -> Maybe Word32
(!?) (TCM n v) (i,j) = v V.!? (i * n + j)


-- | /O(1)/
--
--   The number of rows and columns in the 'TCM'.
{-# INLINE size #-}
size :: TCM -> Int
size (TCM x _) = x


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
fromCols :: (Foldable t, Foldable t', Integral a) => t (t' a) -> TCM
fromCols xs
  | null xs          = error "fromCols: An empty structure was supplied. Cannot construct an empty TCM!"
  | hasJaggedCols    = error jaggedColsErrorMsg
  | width /= height  = error notSquareErrorMsg
  | otherwise        = result
  where
    intermediaryForm = Boxed.fromList $ V.fromList . fmap coerce . toList <$> toList xs
    width            = Boxed.length intermediaryForm
    height           = V.length $ intermediaryForm Boxed.! 0
    hasJaggedCols    = not . equalityOf V.length $ toList intermediaryForm

    jaggedColsErrorMsg = mconcat 
                       [ "fromCols: All the columns did not have the same height! "
                       , "Expected modal height of ("
                       , show mode
                       , ") but found other heights of "
                       , show otherLengths
                       ]
      where
        occuranceMap = Map.fromList . occurances $ V.length <$> intermediaryForm
        (mode,_)     = findMax occuranceMap
        otherLengths = keys  $ mode `delete` occuranceMap
        
    notSquareErrorMsg = mconcat [ "fromRows: The number of rows ("
                                , show height
                                ,") did not match the number of columns ("
                                , show width
                                , ")!"
                                ]
    
    result = TCM height resultVector

    resultVector = V.generate (height * height) f
      where
        f i = (intermediaryForm Boxed.! r) V.! q
         where
           (q,r) = i `divMod` height


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
fromRows :: (Foldable t, Foldable t', Integral a) => t (t' a) -> TCM
fromRows xs
  | null xs          = error "fromRows: An empty structure was supplied. Cannot construct an empty TCM!"
  | hasJaggedRows    = error jaggedRowsErrorMsg
  | width /= height  = error notSquareErrorMsg
  | otherwise        = result
  where
    intermediaryForm = Boxed.fromList $ V.fromList . fmap coerce . toList <$> toList xs
    height           = Boxed.length intermediaryForm
    width            = V.length $ intermediaryForm Boxed.! 0
    hasJaggedRows    = not $ equalityOf V.length intermediaryForm

    jaggedRowsErrorMsg = mconcat 
                       [ "fromRows: All the rows did not have the same width! "
                       , "Expected modal width of ("
                       , show mode
                       , ") but found other widths of "
                       , show otherLengths
                       ]
      where
        occuranceMap = Map.fromList . occurances $ V.length <$> intermediaryForm
        (mode,_)     = findMax occuranceMap
        otherLengths = keys  $ mode `delete` occuranceMap
        
    notSquareErrorMsg = mconcat [ "fromRows: The number of rows ("
                                , show height
                                ,") did not match the number of columns ("
                                , show width
                                , ")!"
                                ]
    
    result = TCM height resultVector

    resultVector = V.generate (height * height) f
      where
        f i = (intermediaryForm Boxed.! q) V.! r
         where
           (q,r) = i `divMod` height


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
generate :: Integral a
         => Int              -- ^ Number of rows & columns in the TCM.
         -> ((Int,Int) -> a) -- ^ Function to determine the value of a given index.
         -> TCM
generate n f
  | n < 1     = error message
  | otherwise = TCM n resultVector
  where
    resultVector = V.generate (n*n) g
      where
        g i = coerce $ f (i `divMod` n)
    message = mconcat
            [ "The call to generate ", show n, " f is malformed, "
            , "the dimension (", show n, ") is a non-positive number!"
            ]


{-# INLINE vec #-}
vec :: TCM -> Vector Word32
vec (TCM _ v) = v


{-# INLINE coerce #-}
coerce :: Integral a => a -> Word32
coerce = toEnum . fromEnum . toInteger

