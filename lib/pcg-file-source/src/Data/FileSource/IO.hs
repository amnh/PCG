-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FileSource.IO
-- Copyright   :  (c) 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Exposes several useful disk utility related functionality.
--
-----------------------------------------------------------------------------

module Data.FileSource.IO
  ( -- * Input
    readFile
    -- * Output
  , appendFile
  , writeFile
  , writeFileWithMove
    -- * Binary data IO
  , deserializeBinary
  , serializeBinary
    -- * Compact region IO
  , deserializeCompact
  , serializeCompact
  ) where

import           Control.DeepSeq                   (NFData)
import           Control.Monad.Trans.Validation
import           Data.Bifunctor                    (first)
import           Data.Binary
--import           Data.Coerce           (Coercible, coerce)
import           Data.FileSource
import           Data.FileSource.InputStreamError
import           Data.FileSource.OutputStreamError
import           Data.Foldable
import           Data.Hashable
import           Data.Key
import           Data.Maybe                        (fromMaybe, isJust, maybe)
import           Data.MonoTraversable
import           Data.MonoTraversable.Keys
import           Data.String
import           Data.Text                         (Text)
import           Data.Text.Short                   (ShortText, pack, unpack)
import qualified Data.Text.Short                   as TS
import           GHC.Generics                      (Generic)
import           Prelude                           hiding (appendFile, readFile, writeFile)
import           Test.QuickCheck                   (Arbitrary (..), CoArbitrary (..))
import           Text.Printf                       (PrintfArg)
import           TextShow


readFile :: FileSource -> ValidationT InputStreamError IO Text
readFile = undefined


serializeBinary :: FileSource -> ValidationT InputStreamError IO a
serializeBinary = undefined


serializeCompact :: FileSource -> ValidationT InputStreamError IO a
serializeCompact = undefined


deserializeBinary :: FileSource -> a -> ValidationT OutputStreamError IO ()
deserializeBinary = undefined


deserializeCompact :: FileSource -> a -> ValidationT OutputStreamError IO ()
deserializeCompact = undefined


appendFile :: FileSource -> Text -> ValidationT OutputStreamError IO ()
appendFile = undefined


writeFile :: FileSource -> Text -> ValidationT OutputStreamError IO ()
writeFile = undefined


writeFileWithMove :: FileSource -> Text -> ValidationT OutputStreamError IO ()
writeFileWithMove = undefined
