-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tuple.Utility
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility functions for pairs
--
-----------------------------------------------------------------------------

module Data.Tuple.Utility
  ( proj3_1
  , proj3_2
  , proj3_3
  , proj4_1
  , proj4_2
  , proj4_3
  , proj4_4
  , proj5_1
  , proj5_2
  , proj5_3
  , proj5_4
  , proj5_5
  ) where

-- |
-- Project from triple onto first component.
proj3_1 :: (a,b,c) -> a
proj3_1 (a,_,_) = a

-- |
-- Project from triple onto second component.
proj3_2 :: (a,b,c) -> b
proj3_2 (_,b,_) = b

-- |
-- Project from triple onto third component.
proj3_3 :: (a,b,c) -> c
proj3_3 (_,_,c) = c


-- |
-- Project from quadruple onto first component.
proj4_1 :: (a,b,c,d) -> a
proj4_1 (a,_,_,_) = a


-- |
-- Project from quadruple onto second component.
proj4_2 :: (a,b,c,d) -> b
proj4_2 (_,b,_,_) = b


-- |
-- Project from quadruple onto third component.
proj4_3 :: (a,b,c,d) -> c
proj4_3 (_,_,c,_) = c


-- |
-- Project from quadruple onto fourth component.
proj4_4 :: (a,b,c,d) -> d
proj4_4 (_,_,_,d) = d


-- |
-- Project from quintuple onto first component.
proj5_1 :: (a,b,c,d,e) -> a
proj5_1 (a,_,_,_,_) = a


-- |
-- Project from quintuple onto second component.
proj5_2 :: (a,b,c,d,e) -> b
proj5_2 (_,b,_,_,_) = b


-- |
-- Project from quintuple onto third component.
proj5_3 :: (a,b,c,d,e) -> c
proj5_3 (_,_,c,_,_) = c


-- |
-- Project from quintuple onto fourth component.
proj5_4 :: (a,b,c,d,e) -> d
proj5_4 (_,_,_,d,_) = d


-- |
-- Project from quintuple onto fifth component.
proj5_5 :: (a,b,c,d,e) -> e
proj5_5 (_,_,_,_,e) = e
