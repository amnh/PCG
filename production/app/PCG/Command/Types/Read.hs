{-# LANGUAGE FlexibleContexts #-}

module PCG.Command.Types.Read
  ( evaluate
  , validate
  ) where

import           Bio.Phylogeny.Graph
import           Bio.Sequence.Parsed
import           Control.Arrow              ((&&&))
import           Control.Monad              (liftM2,when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Evaluation
import           Data.Bifunctor             (first)
import           Data.Char                  (toLower)
import           Data.Either                (partitionEithers)
import           Data.Either.Combinators    (isRight, rightToMaybe)
import           Data.Either.Custom
import           Data.Foldable
import           Data.Map                   (Map)
import           Data.Maybe                 (fromJust,isNothing)
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict  as HM (fromList)
import           Data.Text.Lazy             (Text)
import           File.Format.Fasta
import           File.Format.Fastc   hiding (Identifier)
import           File.Format.Newick
import           File.Format.TransitionCostMatrix
import           File.Format.VertexEdgeRoot
-- import File.Format.Conversion.ToInternal
import           PCG.Command.Types
import           PCG.Command.Types.Read.Evaluate (evaluate)
import           PCG.Command.Types.Read.Internal
import           PCG.Command.Types.Read.Validate (validate)
import           PCG.Script.Types
import           Prelude             hiding (lookup)
