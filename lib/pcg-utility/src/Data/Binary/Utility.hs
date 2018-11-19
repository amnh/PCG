{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Utility
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for collecting 'Left' values of 'Either' forming a 'Validation'
-- similar context.
--
-----------------------------------------------------------------------------

module Data.Binary.Utility
  ( getFieldFromBinary
  )
  where
import Control.Lens.Getter    (Getter)
import Control.Lens.Operators ((^.))
import Data.Binary
import Data.ByteString.Lazy   as BS (readFile)
import System.Directory       (doesFileExist, makeAbsolute)

getFieldFromBinary
  :: forall a s . (Binary s)
  => FilePath -> Getter s a -> IO a
getFieldFromBinary filePath getter =
  do
    fileExist   <- doesFileExist filePath
    absFilePath <- makeAbsolute filePath
    case fileExist of
      False -> error $
                    "No file found with the specified filepath:\n"
                 <> absFilePath
      True -> do
                byteString <- BS.readFile filePath
                let (decodeS :: s) = decode byteString
                pure $ decodeS ^. getter
