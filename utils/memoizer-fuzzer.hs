{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent
import Control.DeepSeq
import Control.Parallel.Strategies
import Control.Parallel.Custom
import Data.Bits
import Data.Functor
import Data.Foldable
import Data.Hashable.Memoize
import Data.List (zip3)
import Data.Time.Clock
import Data.Word
import System.Random
import System.Random.Shuffle
import System.IO.Unsafe


main :: IO ()
main = do
   let !memo =  memoize delayWork -- expensiveFunction
   let !gen  =  sequenceA $ replicate 16 randomIO :: IO [Word]
   args      <- zip3 <$> gen <*> gen <*> gen
   args'     <- force <$> shuffleM args
   !t0       <- getCurrentTime
   let v0    =  sum' . force $ parmap rdeepseq memo args
--   let v0   =   sum . force $ parmap rpar memo args
   print v0
   !t1       <- getCurrentTime
   let v1    =  sum' . force $ parmap rdeepseq memo args'
--   let v1    =  sum' . force $ parmap rpar memo args'
   print v1
   !t2       <- getCurrentTime

   putStr "1st pass: "
   print $ t1 `diffUTCTime` t0
   putStr "2nd pass: "
   print $ t2 `diffUTCTime` t1
   putStrLn $ unwords ["Sums:", show v0, show v1]

   let !m    =  memoize expensiveFunction
   let !allW =  [minBound .. maxBound] :: [Word8]
   let !vals =  [ (x,y,z) | x <- allW, y <- allW, z <- allW ]
   concur    <- traverse shuffleM $ replicate 16 vals
   putStrLn "Beginning concurrent calculations..."
   let v2    = sum' . fmap sum' $ parmap rdeepseq (parmap rdeepseq m) concur

   putStrLn $ "No concurrent collisions!" <> show v2


sum' :: Foldable f => f Word -> Word
sum' = foldl' (+) 0

delayWork (x,_,_) = unsafePerformIO (threadDelay (1000000 + fromEnum (min x 1000000)) $> 0)


expensiveFunction
  :: ( FiniteBits a
     , FiniteBits b
     , FiniteBits c
     )
  => (a, b, c)
  -> Word
expensiveFunction (x, y, z) = f (finiteBitSize x) 0
  where
    f 0 v = v
    f m v = v + g m (finiteBitSize y) 0

    g _ 0 v = v
    g m n v = v + h m n (finiteBitSize z) 0

    h _ _ 0 v = v
    h m n p v
      | xors
          [ xors [ z `testBit` (m - 1), y `testBit` (n - 1), z `testBit` (p - 1)]
          , xors (odd  <$> [(n + p)   *   m, (m + p)   *   n, (n + m)   *   p])
          , xors (even <$> [(n + p) `mod` m, (m + p) `mod` n, (n + m) `mod` p])
          ]       = v + 1
      | otherwise = v + 0

    xors = foldr1 xor
