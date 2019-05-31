-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FileSource
-- Copyright   :  (c) 2015-2018 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Represents a file path.
--
-- However instead of using a string representation as 'FilePath' does, this
-- module exposes a newtype wrapper for 'ShortText'.
--
-- Defines relevant type-classes for converting to and from 'FileSource'.
--
-- Also exposes several useful disk utility related functionality.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.FileSource
  ( FileExtension()
  , FileSource(..)
  , extractExtension
  , toFileSource
  )
  where

import           Control.DeepSeq           (NFData)
import           Data.Bifunctor            (first)
import           Data.Binary
import           Data.Char                 (toLower)
--import           Data.Coerce           (Coercible, coerce)
import           Data.Foldable
import           Data.Hashable
import           Data.Key
import           Data.Maybe                (fromMaybe, isJust, maybe)
import           Data.MonoTraversable
import           Data.MonoTraversable.Keys
import           Data.String
import           Data.Text.Short           (ShortText, pack, unpack)
import qualified Data.Text.Short           as TS
import           GHC.Generics              (Generic)
import           System.FilePath.Posix     (takeExtension)
import           Test.QuickCheck           (Arbitrary (..), CoArbitrary (..))
import           Text.Printf               (PrintfArg)
import           TextShow                  (TextShow (..), fromText)


newtype FileSource = FileSource { toShortText :: ShortText }
    deriving ( Binary
             , Eq
             , Generic
             , Hashable
             , IsString
             , Monoid
             , NFData
             , Ord
             , PrintfArg
             , Read
             , Semigroup
             , Show
             )


newtype FileExtension = FileExtension { unwrapExtension :: ShortText }
    deriving ( Binary
             , Eq
             , Generic
             , Hashable
             , IsString
             , Monoid
             , NFData
             , Ord
             , PrintfArg
             , Read
             , Semigroup
             , Show
             )


type instance Element FileSource = Char


type instance MonoKey FileSource = Word


instance Arbitrary FileSource where

    arbitrary = FileSource . pack <$> arbitrary


instance CoArbitrary FileSource where

    coarbitrary = coarbitrary . unpack . toShortText


instance MonoFoldable FileSource where

    {-# INLINE ofoldMap #-}
    ofoldMap f = TS.foldl' (\x y -> x <> f y) mempty . toShortText

    {-# INLINE ofoldr #-}
    ofoldr f e = TS.foldr f e . toShortText

    {-# INLINE ofoldl' #-}
    ofoldl' f e = TS.foldl' f e . toShortText

    {-# INLINE ofoldr1Ex #-}
    ofoldr1Ex f = maybe errorMessage (uncurry $ TS.foldr f) . TS.uncons . toShortText
      where
        errorMessage = error "call to Data.FileSource.ofoldr1Ex with empty value"

    {-# INLINE ofoldl1Ex' #-}
    ofoldl1Ex' f = maybe errorMessage (uncurry $ TS.foldl' f) . TS.uncons . toShortText
      where
        errorMessage = error "call to Data.FileSource.ofoldl1Ex with empty value"

    {-# INLINE otoList #-}
    otoList = unpack . toShortText

    {-# INLINE oall #-}
    oall f = TS.all f . toShortText

    {-# INLINE oany #-}
    oany f = TS.any f . toShortText

    {-# INLINE onull #-}
    onull = TS.null . toShortText

    {-# INLINE olength #-}
    olength = TS.length . toShortText

    headEx fs =
        case TS.uncons $ toShortText fs of
          Nothing    -> error "call to Data.FileSource.headEx with empty value"
          Just (c,_) -> c

    lastEx fs =
        case TS.unsnoc $ toShortText fs of
          Nothing    -> error "call to Data.FileSource.lastEx with empty value"
          Just (_,c) -> c

    {-# INLINE oelem #-}
    oelem e = isJust . TS.findIndex (==e) . toShortText

    {-# INLINE onotElem #-}
    onotElem e = not . oelem e


instance MonoFoldableWithKey FileSource where

    -- | /O(n)/
    {-# INLINE otoKeyedList #-}
    otoKeyedList = fmap (first toEnum) . toKeyedList . unpack . toShortText

    -- | /O(n)/
    {-# INLINE ofoldMapWithKey #-}
    ofoldMapWithKey f = foldMapWithKey (f . toEnum) . unpack . toShortText

    -- | /O(n)/
    {-# INLINE ofoldrWithKey #-}
    ofoldrWithKey f e = foldrWithKey (f . toEnum) e . unpack . toShortText

    -- | /O(n)/
    {-# INLINE ofoldlWithKey #-}
    ofoldlWithKey f e = foldlWithKey (\v -> f v . toEnum) e . unpack . toShortText


instance MonoFunctor FileSource where

    {-# INLINE omap #-}

    omap f = FileSource . pack . fmap f . unpack . toShortText


instance MonoIndexable FileSource where

    -- | /O(1)/
    {-# INLINE oindex #-}
    oindex fs i = fromMaybe errorMessage $ i `olookup` fs
      where
        errorMessage = error $ fold
            [ "Data.FileSource.oindex: "
            , "The index "
            , show i
            , " was greater than or equal to the length of the file source "
            , show $ olength fs
            ]


instance MonoKeyed FileSource where

    {-# INLINE omapWithKey #-}

    omapWithKey f = FileSource . pack . omapWithKey (f . toEnum) . unpack . toShortText


instance MonoLookup FileSource where

    -- | /O(1)/
    {-# INLINE olookup #-}
    olookup k fs = TS.indexMaybe (toShortText fs) (fromEnum k)


-- | (✔)
instance MonoTraversable FileSource where

    {-# INLINE otraverse #-}

    otraverse f = fmap (FileSource . pack) . traverse f . unpack . toShortText


instance MonoTraversableWithKey FileSource where

    -- | /O(n)/
    {-# INLINE otraverseWithKey #-}
    otraverseWithKey f = fmap (FileSource . pack) . traverseWithKey (f . toEnum) . unpack . toShortText


instance TextShow FileSource where

    showb = fromText . TS.toText . toShortText


-- |
-- /O(n)/
--
-- Takes a structure that container of 'Char's and creates a 'FileSource'.
{-# INLINE[1] toFileSource #-}
toFileSource :: (MonoFoldable s, Element s ~ Char) => s -> FileSource
toFileSource = FileSource . pack . otoList

{-# RULES
"toFileSource/ShortText"     forall (s :: ShortText).       toFileSource s = FileSource s
  #-}


-- |
-- /O(n)/
--
-- Get the normalizied extenstion of a FileSource.
--
-- The normalized form is a lower-case string with no leading '.'.
--
-- Returns @Nothing@ if there is no extension.
extractExtension :: FileSource -> Maybe FileExtension
extractExtension = fmap (FileExtension . fromString) . dropDot . fmap toLower . takeExtension . otoList
  where
    dropDot      []  = Nothing
    dropDot ('.':xs) = Just xs
    dropDot      xs  = Just xs
