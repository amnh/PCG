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
-- Output an XML.Element from various input types.
-- Includes helper fns, all of which are exported.
--
-----------------------------------------------------------------------------

module Text.XML.Custom
  ( ToXML (..)
  , collapseElemList
  , xmlAttr
  , xmlContent
  , xmlElement
  , xmlQName
  ) where


import Text.XML.Class
import Text.XML.Light.Types


-- |
-- Take in a list of ToXML items and return a single Element with XML'ed items as substructure.
-- Used in ToXML instances when there are sequences of data.
--
-- Wanted to make it take any Traversable sequence, but realized that wouldn't guarantee 'toList' was available.
-- TODO: find a way around that?
collapseElemList :: (ToXML a) => String -> [Attr] -> [a] -> Element
collapseElemList name attrs lst = Element (xmlQName name) attrs contents Nothing
    where
        contents = Elem . toXML <$> lst


-- | Create an XML Attr, which is a key value pair (xmlQName, String).
xmlAttr :: (String, String) -> Attr
xmlAttr (name, val) = Attr (xmlQName name) val


-- | Create XML Contents, which for our use is always an Element.
xmlContent :: (String, String) -> Content
xmlContent (key, val) = Elem $ Element (xmlQName key) [] [CRef val] Nothing


-- | Create an XML Element from a String, a list of attributes and a list of contents.
xmlElement :: String -> [(String, String)] -> [(String, Either String Element)] -> Element
xmlElement name attrs contLst = Element (xmlQName name) attributes contents Nothing
    where
        attributes = xmlAttr                 <$> attrs
        contents   = parseElemTupleToContent <$> contLst


-- | Create a QName from a String.
xmlQName :: String -> QName
xmlQName str = QName str mempty mempty


-- | Coerce an input of (String, Either String Element) into Content
parseElemTupleToContent :: (String, Either String Element) -> Content
parseElemTupleToContent  (tag, Left str    ) = xmlContent (tag, str)
parseElemTupleToContent  (_  , Right inElem) = Elem inElem
