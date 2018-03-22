-- // from http://stackoverflow.com/questions/35050825/haskell-ffi-passing-array-of-c-structs-in-and-out

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

module RGB where

import Foreign
import Foreign.C
import Control.Monad (ap)

#include "rgb.h"

data RGB = RGB { r :: CFloat
               , g :: CFloat
               , b :: CFloat
               } deriving Show

instance Storable RGB where
    sizeOf    _ = #{size rgb_t}
    alignment _ = alignment (undefined :: CInt)

    poke p rgb_t = do
      #{poke rgb_t, r} p $ r rgb_t
      #{poke rgb_t, g} p $ g rgb_t
      #{poke rgb_t, b} p $ b rgb_t

    peek p = return RGB
             `ap` (#{peek rgb_t, r} p)
             `ap` (#{peek rgb_t, g} p)
             `ap` (#{peek rgb_t, b} p)

foreign import ccall "rgb.h rgb_test" crgbTest :: Ptr RGB -> CSize -> IO ();

rgbTest :: [RGB] -> IO [RGB]
rgbTest rgbs = withArray rgbs $ \ptr ->
               do
                 crgbTest ptr (fromIntegral (length rgbs))
                 peekArray (length rgbs) ptr

rgbAlloc :: [RGB] -> IO (Ptr RGB)
rgbAlloc rgbs = newArray rgbs

rgbPeek :: Ptr RGB -> Int -> IO [RGB]
rgbPeek rgbs l = peekArray l rgbs

rgbTest2 :: Ptr RGB -> Int -> IO ()
rgbTest2 ptr l =
    do
      crgbTest ptr (fromIntegral l)
      return ()

