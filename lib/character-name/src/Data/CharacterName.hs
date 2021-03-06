------------------------------------------------------------------------------
-- |
-- Module      :  Data.CharacterName
-- Copyright   :  (c) 2015-2021 Ward Wheeler
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
-- A 'CharacterName' can be a either a user specified textual identifier or a
-- default generated value. Whether a given 'CharacterName' is user specified or
-- defaulted may be queried by the function 'isUserDefined'.
--
-- A 'CharacterName' is only constructable by calling 'assignCharacterNames' or
-- makeCharacterNames.
-- Calls to either function should supply all possible character information
-- available at parse time to ensure that the resulting 'CharacterName's are
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

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE UnboxedSums        #-}

module Data.CharacterName
  ( CharacterName()
  -- * Construction
  , assignCharacterNames
  , makeCharacterNames
  -- * Queries
  , isUserDefined
  , sourceFile
  ) where

import           Control.DeepSeq
import           Control.Monad.State.Lazy
import           Data.Binary              (Binary)
import           Data.FileSource
import           Data.Foldable
import           Data.Key                 (lookup)
import           Data.Map                 (Map, insertWith)
import           Data.MonoTraversable
import           Data.Monoid
import           Data.String
import           Data.Text.Short          (ShortText, isPrefixOf, uncons)
import qualified Data.Text.Short          as TS
import           Data.Traversable
import           GHC.Generics             (Generic)
import           Prelude                  hiding (lookup)
import           TextShow                 (TextShow(showb, showbList), toString)
import           TextShow.Data.List       (showbListWith)
import           TextShow.Data.ShortText  ()


-- |
-- Represents the name of a character in a type-safe manner which resolves namespace ambiguities.
data CharacterName
   = UserDefined !FileSource !ShortText
   | Default     !FileSource {-# UNPACK #-} !Word
   deriving stock    (Eq, Generic)
   deriving anyclass (Binary, NFData)


type instance Element CharacterName = Char


instance IsString CharacterName where

    fromString = UserDefined "Unspecified Path" . fromString


instance MonoFoldable CharacterName where

    ofoldMap f   = foldMap f . getList

    ofoldr   f a = foldr f a . getList

    ofoldl'  f a = foldl' f a . getList

    otoList      = getList

    oall     p   = all p . getList

    oany     p   = any p . getList

    onull        = onull . getList

    olength      = length . getList

    ofoldr1Ex f m =
        case getList m of
          []   -> error "Data.MonoTraversable.ofoldr1Ex"
          x:xs -> foldr f x xs

    ofoldl1Ex' f m =
        case getList m of
          []   -> error "Data.MonoTraversable.ofoldl1Ex'"
          x:xs -> foldl' f x xs


-- Ordering biases user defined names with a file path prefix before defaulted names with the same prefix.
instance Ord CharacterName where

    lhs@Default{} `compare` rhs@UserDefined{} =
        case rhs `compare` lhs of
          LT -> GT
          GT -> LT
          EQ -> EQ
    lhs@(UserDefined _ name) `compare` rhs@(Default path _index)
      | toShortText (path <> ":") `isPrefixOf` name = LT
      | otherwise = strCmp lhs rhs
    lhs `compare` rhs = strCmp lhs rhs


instance Show CharacterName where

    show = toString . showb


instance TextShow CharacterName where

    showb (UserDefined _ name) = showb name
    showb (Default path index) = showb path <> ":" <> showb index

    showbList = showbListWith f
      where
        f x = "\"" <> showb x <> "\""


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
sourceFile :: CharacterName -> FileSource
sourceFile (UserDefined x _) = x
sourceFile (Default     x _) = x


getList :: CharacterName -> String
getList (UserDefined _ name) = TS.toString name
getList (Default path index) = otoList path <> ":" <> show index


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
-- >>> makeCharacterNames [("foo.txt", Nothing), ("foo.txt", Just ""), ("foo.txt", Nothing)]
-- ["foo.txt:0","foo.txt:1","foo.txt:2"]
--
makeCharacterNames :: Traversable t => t (FileSource, Maybe ShortText) -> t CharacterName
makeCharacterNames = (`evalState` mempty) . traverse (uncurry assignName)


-- |
-- Assign character names to elements of a container using the provided assignment function.
assignCharacterNames
  :: ( Traversable f
     , Traversable g
     , Traversable h
     )
  => (a -> (FileSource, g (h (Maybe ShortText, b))))
  -> f a
  -> f (g (h (CharacterName, b)))
assignCharacterNames f = (`evalState` mempty) . traverse (g . f)
  where
    g (path, struct) = traverse (traverse (h path)) struct

    h path (may, val) = (\x -> (x, val)) <$> assignName path may


incMap :: Ord a => a -> Map a Word -> Map a Word
incMap k = insertWith (const succ) k 1


validName :: ShortText -> Bool
validName name =
    case uncons name of
      Nothing       -> False
      Just (':', _) -> False
      _             -> True


assignName
  :: MonadState (Map FileSource Word) f
  => FileSource
  -> Maybe ShortText
  -> f CharacterName
assignName path may =
      case may of
        Just name | validName name -> pure $ UserDefined path name
        _ -> do
               seenMap <- get
               modify $ incMap path
               pure $
                 case path `lookup` seenMap of
                   Nothing -> Default path 0
                   Just i  -> Default path i
