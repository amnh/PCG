-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.Parsimony.Dynamic.DirectOptimization.Ukkonen.Ribbon
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Allocates a "ribbon" down the diagonal of the matrix rather than the entire matrix.
--
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns, ConstraintKinds, DeriveFoldable, DeriveFunctor, FlexibleContexts, TypeFamilies #-}

module Analysis.Parsimony.Dynamic.DirectOptimization.Ukkonen.Ribbon
  ( Ribbon()
  , generate
  ) where


import           Data.Foldable
import           Data.Key
import           Data.Semigroup
import           Data.Vector              (Vector)
import qualified Data.Vector       as V
import           Data.Vector.Instances    ()
import           Prelude           hiding (lookup)


-- |
-- Time & space saving data structure for computing only a central "ribbon " of
-- a two dimensional matrix.
data Ribbon a
   = Ribbon
   { height   :: {-# UNPACK #-} !Int 
   , width    :: {-# UNPACK #-} !Int -- width >= height
   , diagonal :: {-# UNPACK #-} !Int
   , offset   :: {-# UNPACK #-} !Int
   , linear   :: Vector a
   } deriving (Eq, Foldable, Functor)


type instance Key Ribbon = (Int, Int)


instance Indexable Ribbon where

    index r k =
      case k `lookup` r of
        Just  v -> v
        Nothing -> error $ mconcat
            [ "Error indexing Ribbon at "
            , show k
            , ".\nThe key is outside the range [ (i,j) | i <- [0 .. "
            , show $ height r - 1
            , "], j <- [0 .. "
            , show $ width  r - 1
            , "] ]"
            ]


instance Lookup Ribbon where

    lookup = ribbonLookup


instance Show (Ribbon a) where

    show (Ribbon h w d a v) = mconcat
        [ "Ribbon { height = "
        , show h
        , ", width = "
        , show w
        , ", diagonal = "
        , show d
        , ", offset = "
        , show a
        , ", |linear| = "
        , show $ length v
        , " } "
        ]


-- |
-- /O( n*m - (m+n-a-1)*(m+n-a) )/ where /n/ is the number of rows, /m/ is the
-- number of columns, and /a/ is the offset from the quasi-diagonal, assuming
-- /m/ > /n/.
--
-- Generates a 'Ribbon' of the specified dimensions, via a generating function,
-- with a specific offset from the quasi-diagonal. The quasi-diagonal is defined
-- as one greater than the difference between the number of rows and the number
-- of columns.
--
-- If the rows > cols of the matrix, then the ribbon is transposed.
generate
  :: Word              -- ^ Rows of the matrix
  -> Word              -- ^ Columns of the matrix
  -> ((Int, Int) -> a) -- ^ Generating function
  -> Word              -- ^ Offset from the quasi-diagonal
  -> Ribbon a
generate x y f alpha = result
  where
    result =
        Ribbon
        { height   = h
        , width    = w
        , diagonal = d
        , offset   = a
        , linear   = vector
        }

    vector      = V.fromListN cellCount $ f <$> points
    diagonalLen = w - h + 1
    cellCount   = h * w - nullCells
    nullCells   = 2 * (t (w - d - a))

    a = min (fromEnum alpha) (w - d)
    d = diagonalLen
    h = fromEnum $ min x y
    w = fromEnum $ max x y

    points =
        [ (i,j)
        | i <- [ 0 .. (h - 1) ]
        , j <- [ max (i - a) 0 .. min (i + d + a - 1) (w-1) ]
        ]


-- |
-- Attempts to index the 'Ribbon' at a point within it's defiend region.
ribbonLookup :: (Int, Int) -> Ribbon a -> Maybe a
ribbonLookup (i,j) r
  | outsideBounds = Nothing
  | otherwise     = Just $ linear r ! k
  where
    k = r `transformation` (i,j)
    h = height r
    w = width  r
    x = j - i
    y = i - j
    upperBarrier = offset r + diagonal r - 1
    lowerBarrier = offset r

    outsideBounds = or
        [ i < 0 || h <= i
        , j < 0 || w <= j
        , x > upperBarrier
        , y > lowerBarrier
        ]

-- |
-- Convert a 2D point to it's linear position in the vector.
--
-- Will produce undefined behavior when transforming a point outside the 'Ribbon'.
transformation :: Ribbon a -> (Int, Int) -> Int
transformation r (i,j) = rowPrefix + colIndex
  where
    a = offset r
    colIndex  = j - max 0 (i - a)
    rowPrefix = i * (d + 2*a) - v - w
      where
        v = t a - t b
        w = t c
        d = diagonal r
        b = max 0 (a - i)
        c = max 0 (i - h + a)

    h = height   r


{-# INLINE t #-}
t :: Int -> Int
t n = (n*(n+1)) `div` 2

