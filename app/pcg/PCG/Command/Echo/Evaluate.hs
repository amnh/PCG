------------------------------------------------------------------------------
-- |
-- Module      :  PCG.Command.Echo.Evaluate
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module PCG.Command.Echo.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Monad.IO.Class (liftIO)
import Data.Functor           (($>))
import Data.Text.IO           (putStrLn)
import Data.Text.Short        (toText)
import PCG.Command.Echo
import Prelude                hiding (putStrLn)


-- |
-- Evaluates an ECHO command by printing the supplied message to the output log.
evaluate :: EchoCommand -> GraphState -> SearchState
evaluate (EchoCommand message) g = liftIO $ putStrLn (toText message) $> g
