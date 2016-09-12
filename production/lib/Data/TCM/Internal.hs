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

{-# LANGUAGE BangPatterns, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.TCM.Internal where

import           Data.Foldable
import           Data.Key
import           Data.List.Utility (equalityOf, occurances)
import           Data.Map          (delete, findMax, keys)
import qualified Data.Map            as Map   (fromList)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid
import qualified Data.Vector         as Boxed
import           Data.Vector.Unboxed          (Unbox, Vector)
import qualified Data.Vector.Unboxed as V
import           Prelude             hiding   (lookup)
import           Test.QuickCheck


-- | A data structure for storing a two dimensional array of bits.
--   Exposes row based monomorphic mapping & folding.
--
--   It is important to note the endianness of 'BitMatrix'.
--   The bit at position @(0,0)@ is displayed in the upper left hand corner when
--   the 'BitMatrix' is shown.
--   The bit at position @(i,x)@ will be of less significance than position @(i+1,x)@,
--   for the resulting xth 'BitVector' row when calling 'rows' on a 'BitMatrix'.
--   The bit at position @(x,i)@ will be of less significance than position @(x,i+1)@,
--   for the resulting xth 'BitVector' column when calling 'cols' on a 'BitMatrix'.
--
data TCM a
   = TCM !Int (Vector a)
   deriving (Eq)

-- | The number of rows and columns in the 'BitMatrix'
--   /O(1)/
size :: TCM a -> Int
size (TCM x _) = x

-- | A generating function for a 'BitMatrix'. Efficiently constructs a
--   'BitMatrix' of the specified dimensions with each bit defined by the result
--   of the supplied function.
--
--   /O(n*n)/
--
-- ==== __Examples__
--
generate :: Integral a
         => Int              -- ^ Number of rows & columns in the TCM.
         -> ((Int,Int) -> a) -- ^ Function to determine the value of a given index.
         -> TCM a
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

{-# INLINE coerce #-}
coerce :: Integral a => a -> Int
coerce = fromEnum . toInteger

-- | Construct a 'TCM' from a list of rows.
--   /O(n*n)/
fromRows :: (Foldable t, Foldable t', Unbox a) => t (t' a) -> TCM a
fromRows xs
  | null xs          = error "fromRows: An empty structure was supplied. Cannot construct an empty TCM!"
  | hasJaggedRows    = error jaggedRowsErrorMsg
  | width /= height  = error notSquareErrorMsg
  | otherwise        = result
  where
    intermediaryForm = Boxed.fromList $ V.fromList . toList <$> toList xs
    height           = Boxed.length intermediaryForm
    width            = V.length $ intermediaryForm Boxed.! 0
    hasJaggedRows    = not $ equalityOf V.length intermediaryForm

    jaggedRowsErrorMsg = mconcat 
                       [ "fromRows: All the rows did not have the same width! "
                       , "Expected width modal width of ("
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


-- | Construct a 'TCM' from a list of columns.
--   /O(n*n)/
fromCols :: (Foldable t, Foldable t', Unbox a) => t (t' a) -> TCM a
fromCols xs
  | null xs          = error "fromCols: An empty structure was supplied. Cannot construct an empty TCM!"
  | hasJaggedCols    = error jaggedColsErrorMsg
  | width /= height  = error notSquareErrorMsg
  | otherwise        = result
  where
    intermediaryForm = Boxed.fromList $ V.fromList . toList <$> toList xs
    width            = Boxed.length intermediaryForm
    height           = V.length $ intermediaryForm Boxed.! 0
    hasJaggedCols    = not . equalityOf V.length $ toList intermediaryForm

    jaggedColsErrorMsg = mconcat 
                       [ "fromCols: All the rows did not have the same width! "
                       , "Expected width modal width of ("
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
        f i = (intermediaryForm Boxed.! r) V.! q
         where
           (q,r) = i `divMod` height

{-# INLINE vec #-}
vec :: TCM a -> Vector a
vec (TCM _ v) = v

type instance Key TCM = (Int, Int)

instance Indexable TCM where
  {-# INLINE index #-}
  index a i = fromMaybe raiseError $ i `lookup` a
    where
      raiseError = error $ mconcat
                 ["Error indexing Alphabet at location "
                 , show i
                 , ", valid inclusive index range is [0, "
                 , show $ length a - 1
                 , "]."
                 ]


instance Lookup TCM where
  {-# INLINE lookup #-}
  (i,j) `lookup` (TCM n v) = v V.!? (i * n + j)

  
-- | Performs a row-wise monomporphic fold over ther 'BitMatrix'.
instance Foldable TCM where
  -- | Map each element of a structure to a 'Monoid' and combine the results.
  {-# INLINE foldMap #-}
  foldMap f = V.foldr (mappend . f) mempty

  -- | Right-associative fold of a structure.
  {-# INLINE foldr #-}
  foldr f e = V.foldr f e . vec

  -- | Strict right-associative fold of a structure.
  {-# INLINE foldr' #-}
  foldr' f e = V.foldr' f e . vec

  -- | Left-associative fold of a structure.
  {-# INLINE foldl #-}
  foldl f e = V.foldl f e . vec

  -- | Strict left-associative fold of a structure.
  {-# INLINE foldl' #-}
  foldl' f e = V.foldl' f e . vec

  -- | Right-associative fold of a non-empty structure.
  {-# INLINE foldr1 #-}
  foldr1 f = V.foldr1 f . vec

  -- | Left-associative fold of a non-empty structure.
  {-# INLINE foldl1 #-}
  foldl1 f = V.foldl1 f . vec

  {-# INLINE toList #-}
  toList = V.toList . vec

  {-# INLINE null #-}
  null = const False

  {-# INLINE length #-}
  length = V.length .vec

  {-# INLINE elem #-}
  elem x = V.elem x . vec

  {-# INLINE maximum #-}
  maximum = V.maximum . vec

  {-# INLINE minimum #-}
  minimum = V.minimum . vec

  {-# INLINE sum #-}
  sum = V.sum . vec

  {-# INLINE product #-}
  product = V.product . vec


instance FoldableWithKey TCM where
  {-# INLINE foldrWithKey #-}
  foldrWithKey f e (TCM n v) = V.ifoldr' g e v
    where
      g i = f (i `divMod` n)

  {-# INLINE foldlWithKey #-}
  foldlWithKey f e (TCM n v) = V.ifoldl' g e v
    where
      g i = f (i `divMod` n)

{-
-- | Resulting matricies will have at /least/ one row and one column.
instance Arbitrary BitMatrix where
    arbitrary = do 
        colCount <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 20) 
        rowCount <- (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= 20)
        let rVal = choose (0, 2 ^ colCount -1) :: Gen Integer
        bitRows  <- vectorOf rowCount rVal
        pure . fromRows $ bitVec colCount <$> bitRows

-- | (✔)
instance Show BitMatrix where
    show bm = headerLine <> matrixLines
      where
        renderRow   = foldl (\acc e -> (if e then '1' else '0') : acc) "" . toBits
        matrixLines = unlines $ renderRow <$> rows bm          
        headerLine  = '\n' : unwords
                    [ "BitMatrix:"
                    , show $ numRows bm
                    , "x"
                    , show $ numCols bm
                    , "\n"
                    ]
-}
