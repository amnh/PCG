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


import Data.Foldable
import Text.XML.Light.Types


-- |
class ToXML a where

    toXML :: a -> Element


instance (ToXML a) => ToXML (Maybe a) where

    toXML input = Element (QName "Maybe_data" Nothing Nothing) [] [content] Nothing
        where
            content = case input of
                          Nothing  -> CRef "No_data"
                          Just val -> Elem $ toXML val


instance ToXML [Char] where

    toXML val = Element (QName "Text_value" Nothing Nothing) [] [CRef val] Nothing


instance {-# OVERLAPPABLE #-} (Foldable f, ToXML a) => ToXML (f a) where

    toXML lst = Element name attrs contents Nothing
       where
           name     = QName "List" Nothing Nothing
           attrs    = []
           contents = Elem . toXML <$> toList lst

