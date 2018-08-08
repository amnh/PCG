---------------------------------------------------------------------------
-- |
-- Module      :  Text.XML
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


module Text.XML
    ( ToXML (..)
    , Content (CRef)
    , collapseElemList
    , ppTopElement
    , xmlAttr
    , xmlContent
    , xmlElement
    , xmlQName
    ) where


import           Text.XML.Class
import           Text.XML.Light
