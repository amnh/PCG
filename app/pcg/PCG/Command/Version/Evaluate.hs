module PCG.Command.Version.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Monad.IO.Class (liftIO)
import Data.Functor           (($>))
import PCG.Command.Version
import PCG.Software.Metadata

evaluate :: VersionCommand -> GraphState -> SearchState
evaluate (VersionCommand printFullVersion) graphState =
    liftIO (putStrLn versionStr) $> graphState
  where
    versionStr
      | printFullVersion = fullVersionInformation
      | otherwise        = shortVersionInformation
