module Data.Matrix.NotStupid
  ( matrix
  , getElem
  , (!)
  , (<->)
  , (<|>)
  , unsafeGet
  , safeGet
  , getRow
  , getCol
  , setElem
  , unsafeSet
  , mapRow
  , mapCol
  , submatrix
  , minorMatrix
  , splitBlocks
  , scaleRow
  , combineRows
  , switchRows
  , switchCols
  , Stupid.Matrix ()
  , Stupid.prettyMatrix
  , Stupid.nrows
  , Stupid.ncols
  , Stupid.forceMatrix
    -- * Builders
  , Stupid.rowVector
  , Stupid.colVector
    -- ** Special matrices
  , Stupid.zero
  , Stupid.identity
  , Stupid.permMatrix
    -- * List conversions
  , Stupid.fromList
  , Stupid.fromLists
  , Stupid.toList
  , Stupid.toLists
    -- * Accessing
  , Stupid.getDiag
  , Stupid.getMatrixAsVector
    -- * Manipulating matrices
  , Stupid.transpose
  , Stupid.setSize
  , Stupid.extendTo
  -- * Submatrices
  -- ** Joining blocks
  , Stupid.joinBlocks
    -- * Matrix operations
  , Stupid.elementwise
  , Stupid.elementwiseUnsafe
-- * Matrix multiplication
-- ** About matrix multiplication
-- $mult
  -- ** Functions
  , Stupid.multStd
  , Stupid.multStd2
  , Stupid.multStrassen
  , Stupid.multStrassenMixed
  -- * Linear transformations
  , Stupid.scaleMatrix
  -- * Decompositions
  , Stupid.luDecomp
  , Stupid.luDecompUnsafe
  , Stupid.luDecomp'
  , Stupid.luDecompUnsafe'
  , Stupid.cholDecomp
    -- * Properties
  , Stupid.trace
  , Stupid.diagProd
  -- ** Determinants
  , Stupid.detLaplace
  , Stupid.detLU
  ) where

import           Control.Arrow ((***))
import           Data.Matrix   (Matrix,(<->),(<|>))
import qualified Data.Matrix as Stupid 
import           Data.Vector   (Vector)


-- | /O(rows*cols)/. Generate a matrix from a generator function.
--   Example of usage:
--
-- >                                  (  1  0 -1 -2 )
-- >                                  (  3  2  1  0 )
-- >                                  (  5  4  3  2 )
-- > matrix 4 4 $ \(i,j) -> 2*i - j = (  7  6  5  4 )
matrix :: Int -- ^ Rows
       -> Int -- ^ Columns
       -> ((Int,Int) -> a) -- ^ Generator function
       -> Matrix a
{-# INLINE matrix #-}
matrix m n f = Stupid.matrix m n (f . (pred *** pred))

-------------------------------------------------------
-------------------------------------------------------
---- ACCESSING

-- | /O(1)/. Get an element of a matrix. Indices range from /(1,1)/ to /(n,m)/.
--   It returns an 'error' if the requested element is outside of range.
getElem :: Int      -- ^ Row
        -> Int      -- ^ Column
        -> Matrix a -- ^ Matrix
        -> a
{-# INLINE getElem #-}
getElem i j = Stupid.getElem (i+1) (j+1)

-- | An infix alias for 'getElem' which takes a tuple.
(!) :: Matrix a -> (Int,Int) -> a
{-# INLINE (!) #-}
m ! (i,j) = getElem i j m

-- | /O(1)/. Unsafe variant of 'getElem', without bounds checking.
unsafeGet :: Int      -- ^ Row
          -> Int      -- ^ Column
          -> Matrix a -- ^ Matrix
          -> a
{-# INLINE unsafeGet #-}
unsafeGet i j = Stupid.unsafeGet (i+1) (j+1)

-- | Variant of 'getElem' that returns Maybe instead of an error.
safeGet :: Int -> Int -> Matrix a -> Maybe a
safeGet i j = Stupid.safeGet (i+1) (j+1)

-- | /O(1)/. Get a row of a matrix as a vector.
getRow :: Int -> Matrix a -> Vector a
{-# INLINE getRow #-}
getRow i = Stupid.getRow (i+1)

-- | /O(rows)/. Get a column of a matrix as a vector.
getCol :: Int -> Matrix a -> Vector a
{-# INLINE getCol #-}
getCol j = Stupid.getCol (j+1)

-- | Replace the value of a cell in a matrix.
setElem :: a -- ^ New value.
        -> (Int,Int) -- ^ Position to replace.
        -> Matrix a -- ^ Original matrix.
        -> Matrix a -- ^ Matrix with the given position replaced with the given value.
{-# INLINE setElem #-}
setElem e (i,j) = Stupid.setElem e (i+1,j+1)

-- | Unsafe variant of 'setElem', without bounds checking.
unsafeSet :: a -- ^ New value.
        -> (Int,Int) -- ^ Position to replace.
        -> Matrix a -- ^ Original matrix.
        -> Matrix a -- ^ Matrix with the given position replaced with the given value.
{-# INLINE unsafeSet #-}
unsafeSet e (i,j) = Stupid.unsafeSet e (i+1,j+1)


-- | /O(rows*cols)/. Map a function over a row.
--   Example:
--
-- >                          ( 1 2 3 )   ( 1 2 3 )
-- >                          ( 4 5 6 )   ( 5 6 7 )
-- > mapRow (\_ x -> x + 1) 2 ( 7 8 9 ) = ( 7 8 9 )
--
mapRow :: (Int -> a -> a) -- ^ Function takes the current column as additional argument.
       -> Int             -- ^ Row to map.
       -> Matrix a -> Matrix a
mapRow f r = Stupid.mapCol (f <$> (+1)) (r+1)

-- | /O(rows*cols)/. Map a function over a column.
--   Example:
--
-- >                          ( 1 2 3 )   ( 1 3 3 )
-- >                          ( 4 5 6 )   ( 4 6 6 )
-- > mapCol (\_ x -> x + 1) 2 ( 7 8 9 ) = ( 7 9 9 )
--
mapCol :: (Int -> a -> a) -- ^ Function takes the current row as additional argument.
       -> Int             -- ^ Column to map.
       -> Matrix a -> Matrix a
mapCol f c = Stupid.mapCol (f <$> (+1)) (c+1)


-- | /O(1)/. Extract a submatrix given row and column limits.
--   Example:
--
-- >                   ( 1 2 3 )
-- >                   ( 4 5 6 )   ( 2 3 )
-- > submatrix 1 2 2 3 ( 7 8 9 ) = ( 5 6 )
submatrix :: Int    -- ^ Starting row
          -> Int -- ^ Ending row
          -> Int    -- ^ Starting column
          -> Int -- ^ Ending column
          -> Matrix a
          -> Matrix a
{-# INLINE submatrix #-}
submatrix r1 r2 c1 c2 = Stupid.submatrix (r1+1) (r2+2) (c1+1) (c2+1)

-- | /O(rows*cols)/. Remove a row and a column from a matrix.
--   Example:
--
-- >                 ( 1 2 3 )
-- >                 ( 4 5 6 )   ( 1 3 )
-- > minorMatrix 2 2 ( 7 8 9 ) = ( 7 9 )
minorMatrix :: Int -- ^ Row @r@ to remove.
            -> Int -- ^ Column @c@ to remove.
            -> Matrix a -- ^ Original matrix.
            -> Matrix a -- ^ Matrix with row @r@ and column @c@ removed.
minorMatrix r c = Stupid.minorMatrix (r+1) (c+1)

-- | /O(1)/. Make a block-partition of a matrix using a given element as reference.
--   The element will stay in the bottom-right corner of the top-left corner matrix.
--
-- >                 (             )   (      |      )
-- >                 (             )   ( ...  | ...  )
-- >                 (    x        )   (    x |      )
-- > splitBlocks i j (             ) = (-------------) , where x = a_{i,j}
-- >                 (             )   (      |      )
-- >                 (             )   ( ...  | ...  )
-- >                 (             )   (      |      )
--
--   Note that some blocks can end up empty. We use the following notation for these blocks:
--
-- > ( TL | TR )
-- > (---------)
-- > ( BL | BR )
--
--   Where T = Top, B = Bottom, L = Left, R = Right.
--
splitBlocks :: Int      -- ^ Row of the splitting element.
            -> Int      -- ^ Column of the splitting element.
            -> Matrix a -- ^ Matrix to split.
            -> (Matrix a,Matrix a
               ,Matrix a,Matrix a) -- ^ (TL,TR,BL,BR)
{-# INLINE[1] splitBlocks #-}
splitBlocks i j  = Stupid.splitBlocks (i+1) (j+1)

-- | Scale a row by a given factor.
--   Example:
--
-- >              ( 1 2 3 )   (  1  2  3 )
-- >              ( 4 5 6 )   (  8 10 12 )
-- > scaleRow 2 2 ( 7 8 9 ) = (  7  8  9 )
scaleRow :: Num a => a -> Int -> Matrix a -> Matrix a
scaleRow e r = Stupid.scaleRow e (r+1)

-- | Add to one row a scalar multiple of another row.
--   Example:
--
-- >                   ( 1 2 3 )   (  1  2  3 )
-- >                   ( 4 5 6 )   (  6  9 12 )
-- > combineRows 2 2 1 ( 7 8 9 ) = (  7  8  9 )
combineRows :: Num a => Int -> a -> Int -> Matrix a -> Matrix a
combineRows r1 l r2 = Stupid.combineRows (r1+1) l (r2+1)

-- | Switch two rows of a matrix.
--   Example:
--
-- >                ( 1 2 3 )   ( 4 5 6 )
-- >                ( 4 5 6 )   ( 1 2 3 )
-- > switchRows 1 2 ( 7 8 9 ) = ( 7 8 9 )
switchRows :: Int -- ^ Row 1.
           -> Int -- ^ Row 2.
           -> Matrix a -- ^ Original matrix.
           -> Matrix a -- ^ Matrix with rows 1 and 2 switched.
switchRows r1 r2 = Stupid.switchRows (r1+1) (r2+1)

-- | Switch two coumns of a matrix.
--   Example:
--
-- >                ( 1 2 3 )   ( 2 1 3 )
-- >                ( 4 5 6 )   ( 5 4 6 )
-- > switchCols 1 2 ( 7 8 9 ) = ( 8 7 9 )
switchCols :: Int -- ^ Col 1.
           -> Int -- ^ Col 2.
           -> Matrix a -- ^ Original matrix.
           -> Matrix a -- ^ Matrix with cols 1 and 2 switched.
switchCols c1 c2 = Stupid.switchCols (c1+1) (c2+1)

