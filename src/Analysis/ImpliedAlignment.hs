-----------------------------------------------------------------------------
-- |
-- Module      :  Analysis.ImpliedAlignment
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard algorithm for implied alignment
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Analysis.ImpliedAlignment
  ( iaSolution
  ) where

import           Analysis.ImpliedAlignment.DynamicProgramming
