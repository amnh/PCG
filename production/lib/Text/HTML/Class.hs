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


module Text.HTML.Class where


import Text.Blaze.Html


-- |
class ToHTML a where

    ToHTML :: a -> Markup


instance (ToHTML a) => ToHTML (Maybe a) where

    toHTML input = lazyText
        where
            content = case input of
                          Nothing  -> CRef "No data"
                          Just val -> Elem $ toXML val


instance ToHTML [Char] where

    toHTML val = Element (QName "Text value" Nothing Nothing) [] [CRef val] Nothing