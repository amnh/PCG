---------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Output tree structure and node decoration data to XML.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleInstances #-}


module Text.XML.Class where


-- import Data.List.NonEmpty
import Bio.Graph.LeafSet
import Data.Foldable
import Data.Monoid
-- import Text.XML.Custom
import Text.XML.Light.Types


-- |
class ToXML a where

    toXML :: a -> Element


instance {-# OVERLAPS #-} (ToXML a) => ToXML (Maybe a) where

    toXML input = Element (QName "Maybe data" Nothing Nothing) [] [content] Nothing
        where
            content = case input of
                          Nothing  -> CRef "No data"
                          Just val -> Elem $ toXML val


instance {-# OVERLAPPING #-} ToXML (LeafSet (Maybe String)) where

    toXML (LeafSet lst) = Element (QName "Leaf set" mempty mempty) attrs contents Nothing
        where
            attrs    = []
            contents = [Elem $ Element (QName "Leaves" mempty mempty) attrs [CRef $ foldr leafStr "" lst] Nothing]

            leafStr input accum = case input of Just item -> item <> ", " <> accum
                                                Nothing   -> accum

-- data StringThing a = ST a


-- -- data StringThing a = ST { val :: String }


-- instance Foldable StringThing where

--     foldMap f (ST input)     = foldMap f input

--     foldr f accum (ST input) = foldr f accum input


instance {-# OVERLAPPING #-} ToXML [Char] where

    toXML val = Element (QName "Text value" Nothing Nothing) [] [CRef val] Nothing


instance {-# OVERLAPPABLE #-} (Foldable f, ToXML a) => ToXML (f a) where

    toXML lst = Element name attrs contents Nothing
        where
            -- (name, attrs, contents) = case lst of -- ST val   -> ("Text val", [], [CRef val]        )
            --                                       Just val -> ("Data"    , [], [Elem $ toXML val])
            --                                       Nothing  -> ("Nothing" , [], [CRef "No data"]  )

            name     = QName "List" mempty mempty
            attrs    = []
            contents = Elem . toXML <$> toList lst
