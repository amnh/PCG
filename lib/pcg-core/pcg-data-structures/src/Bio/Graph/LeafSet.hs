------------------------------------------------------------------------------
-- |
-- Module      :  Bio.Graph.LeafSet
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides a class for HasLeafSet, as well as an instance of HasLeafSet
-- for NonEmptyList
--
-----------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


module Bio.Graph.LeafSet
  ( LeafSet(..)
  , HasLeafSet (..)
  , fromLeafSet
  ) where


import Control.Lens
import Data.List       (union)
--import Text.Newick.Class ()
import Text.XML.Custom ()
import Data.Vector
import Data.Coerce


-- |
-- Set of unique leaf labels.
newtype LeafSet n = LeafSet [n]
    deriving (Foldable, Functor, Show)


-- |
-- A 'Lens' for 'LeafSet'
class HasLeafSet s a | s -> a where

    {-# MINIMAL leafSet #-}
    leafSet :: Lens' s a


-- | (✔)
instance Eq n => Semigroup (LeafSet n) where

    (<>) (LeafSet lhs) (LeafSet rhs) = LeafSet $ union lhs rhs


fromLeafSet :: forall n .  LeafSet n -> Vector n
fromLeafSet = coerce $ fromList @n
{--
instance ToXML (LeafSet (Maybe String)) where

    toXML (LeafSet lst) = xmlElement "Leaf_set" attrs contents
        where
            attrs    = []
            contents = [Left ("Leaves", foldr leafStr "" lst)]

            leafStr input accum = case input of Just item -> item <> ", " <> accum
                                                Nothing   -> accum
--}
