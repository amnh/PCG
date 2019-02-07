------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Metadata.CharacterName
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'CharacterName' represents an /unique/ name identifier for a character that
-- can be used as a textual reference.
--
-- A 'CharacterName' contains within it a reference to the file from which it
-- was originally parsed, allowing identically named characters from different
-- input files to be uniquely referenced. The file from which a 'CharacterName'
-- was originally parsed can be queried by the function 'sourceFile'.
--
-- A 'CharcterName' can be a either a user specified textual identifier or a
-- default generated value. Where a given 'CharacterName' is user specified or
-- defaulted may be queried by the function 'isUserDefined'.
--
-- A 'CharacterName' is only constructable by calling 'makeCharacterNames'.
-- Calls to 'makeCharacterNames' should supply all possible character information
-- available at parse time to ensure that the resulting 'CharacterNames' are
-- uniquely idenitfible.
--
-- A 'CharacterName' has an ordering defined as follows:
--
--  1. The lexical ordering of the 'CharacterName' source file name
--  2. User defined values take precedence over a defaulted values within the same source file
--  3. The lexical ordering of the user defined textual identifier within the same source file
--  4. The numeric ordering of the defaulted values within the same source file.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedSums       #-}


module Bio.Metadata.CharacterName
  ( CharacterName()
  , isUserDefined
  , makeCharacterNames
  , sourceFile
  ) where

import Control.DeepSeq
import Control.Monad.State.Lazy
import Data.Map                 hiding (null)
import Data.Monoid
import Data.String
import Data.Text.Short          (ShortText, isPrefixOf, uncons)
-- TODO: Remove when text-show-instances is updated
import Data.Text.Short.Custom
import Data.Traversable
import GHC.Generics             (Generic)
import Prelude                  hiding (lookup)
import Text.Show                (showListWith, showString)
import TextShow                 (TextShow (showb, showbList), toString)
import TextShow.Data.List       (showbListWith)


-- |
-- Represents the name of a character in a type-safe manner which resolves namespace ambiguities.
data CharacterName
   = UserDefined !FilePath !ShortText
   | Default     !FilePath {-# UNPACK #-} !Int
   deriving (Eq, Generic)


instance IsString CharacterName where

    fromString = UserDefined "Unspecified Path" . fromString


instance NFData CharacterName


-- A custom 'Show' instance for more legible rendering of lists.
instance Show CharacterName where

    show = toString . showb


-- A custom 'TextShow' instance that agrees with the 'Show' instance.
instance TextShow CharacterName where

    showb (UserDefined _ name) = showb name
    showb (Default path index) = showb path <> ":" <> showb index

    showbList = showbListWith f
      where
        f x = "\"" <> showb x <> "\""


-- Ordering biases user defined names with a file path prefix before defaulted names with the same prefix.
instance Ord CharacterName where

  lhs@Default{} `compare` rhs@UserDefined{} =
      case rhs `compare` lhs of
        LT -> GT
        GT -> LT
        EQ -> EQ
  lhs@(UserDefined _ name) `compare` rhs@(Default path _index)
    | (fromString path <> ":") `isPrefixOf` name = LT
    | otherwise = strCmp lhs rhs
  lhs `compare` rhs = strCmp lhs rhs


-- Used internally for ordering logic after special cases are checked.
strCmp :: Show a => a -> a -> Ordering
strCmp lhs rhs = show lhs `compare` show rhs


-- |
-- Determine if the CharacterName was user defined or defaulted.
isUserDefined :: CharacterName -> Bool
isUserDefined UserDefined{} = True
isUserDefined _             = False


-- |
-- Determine the source file for the character.
sourceFile :: CharacterName -> FilePath
sourceFile (UserDefined x _) = x
sourceFile (Default     x _) = x


-- |
-- Construct many 'CharacterName's for the input list of '(FilePath, Maybe String)' pairs.
-- 'Just' values represent user supplied character names while 'Nothing' values represent
-- character names to be defaulted. However, in some cases 'Just' valued strings may be
-- rejected and defaulted instead. This occurs when the user defined character name starts
-- with a ":" or is empty.
--
--   The construction is order preserving on the input structure. Multiple characters from a
--   ffrom the same input file will be defualted with incrementing indices.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> makeCharacterNames [("file.dat", Nothing)]
-- ["file.dat:0"]
--
-- >>> makeCharacterNames [("file.dat", Just "Eyes")]
-- ["Eyes"]
--
-- >>> makeCharacterNames [("path/to/file.dat", Nothing")]
-- ["file.dat:0"]
--
-- >>> makeCharacterNames [("path/to/file.dat", Just "Eyes")]
-- ["Eyes"]
--
-- >>> makeCharacterNames [("file.dat", Nothing), ("file.dat", Nothing)]
-- ["file.dat:0","file.dat:1"]
--
-- >>> makeCharacterNames [("file.dat", Nothing), ("file.dat", Just "Eyes"), ("file.dat", Nothing)]
-- ["file.dat:0","Eyes","file.dat:1"]
--
-- >>> makeCharacterNames [("foo.txt", Nothing), ("foo.txt", Nothing), ("bar.txt", Nothing), ("baz.txt", Nothing), ("baz.txt", Nothing)]
-- ["foo.txt:0","foo.txt:1","bar.txt:0","baz.txt:0","baz.txt"]
--
-- >>> makeDefaultCharacterNameRange ("virus.exe", Just "")
-- ["virus.exe:0"]
--
-- >>> makeDefaultCharacterNameRange ("virus.exe", Just ":")
-- ["virus.exe:0"]
--
-- >>> makeDefaultCharacterNameRange ("virus.exe", Just ":a")
-- ["virus.exe:0"]
--
-- >>> makeDefaultCharacterNameRange ("virus.exe", Just "a:")
-- ["a:"]
--
-- >>> makeDefaultCharacterNameRange ("virus.exe", Just "%#:!")
-- ["%#:!"]
--
-- >>> makeCharacterNames [("foo.txt", Nothing), ("foo.tx", Just ""), ("foo.tx", Nothing)]
-- ["foo.txt:0","foo.txt:1","foo.txt:2"]
--
makeCharacterNames :: Traversable t => t (FilePath, Maybe ShortText) -> t CharacterName
makeCharacterNames = (`evalState` mempty) . mapM f
  where
    f (path, may) =
      case may of
        Just name | validName name -> pure $ UserDefined path name
        _ -> do
               im <- get
               _  <- put $ incMap path im
               pure $
                 case path `lookup` im of
                   Nothing -> Default path 0
                   Just i  -> Default path i

    incMap :: Ord a => a -> Map a Int -> Map a Int
    incMap k = insertWith g k 1
      where
        g = const succ

    validName :: ShortText -> Bool
    validName name =
        case uncons name of
          Nothing       -> False
          Just (':', _) -> False
          _             -> True
