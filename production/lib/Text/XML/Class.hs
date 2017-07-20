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


import Text.XML.Light.Types


-- |
class ToXML a where

    toXML :: a -> Element


instance (ToXML a) => ToXML (Maybe a) where

    toXML input = Element (QName "Maybe data" Nothing Nothing) [] [content] Nothing
        where
            content = case input of
                          Nothing  -> CRef "No data"
                          Just val -> Elem $ toXML val


instance ToXML [Char] where

    toXML val = Element (QName "Text value" Nothing Nothing) [] [CRef val] Nothing