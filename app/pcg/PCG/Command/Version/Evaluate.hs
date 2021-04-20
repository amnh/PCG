------------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Version.Evaluate
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module PCG.Command.Version.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Monad.IO.Class (liftIO)
import Data.Functor           (($>))
import PCG.Command.Version
import PCG.Software.Metadata


-- |
-- Evaluate the 'VersionCommand'.
--
-- Will print the version of PCG.
-- The printed version can either be a brief version or a full version.
evaluate :: VersionCommand -> GraphState -> SearchState
evaluate (VersionCommand printFullVersion) graphState =
    liftIO (putStrLn versionStr) $> graphState
  where
    versionStr
      | printFullVersion = fullVersionInformation
      | otherwise        = shortVersionInformation
