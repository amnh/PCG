---------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Custom
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


{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}


module Text.XML.Custom
  ( ToXML (..)
  , Content (CRef)
  , collapseElemList
  , xmlAttr
  , xmlContent
  , xmlElement
  , xmlQName
  ) where


import Data.Foldable
import Data.Key
-- import Data.Monoid         ((<>))
import Text.XML.Class (ToXML(..))
import Text.XML.Light.Types


-- |
-- Take in a list of ToXML items and return a single Element with XML'ed items as substructure.
-- Used in ToXML instances when there are sequences of data.
collapseElemList :: (FoldableWithKey f, Show (Key f), ToXML a) => String -> [Attr] -> f a -> Element
collapseElemList name attrs lst = Element (xmlQName name) attrs contents Nothing
    where
        -- contents     = (Elem numberElem) : ((Elem . toXML) <$> toList lst)
        numberElem i = Element (xmlQName "Number") [] [CRef $ show i] Nothing
        contents     = Elem <$> toList (foldMapWithKey f lst)
        f i e        = [numberElem i, toXML e]


-- | Create an XML Attr, which is a key value pair (xmlQName, String).
xmlAttr :: (String, String) -> Attr
xmlAttr (name, val) = Attr (xmlQName name) val


-- | Create XML Contents, which for our use is always an Element.
xmlContent :: (String, String) -> Content
xmlContent (key, val) = Elem $ Element (xmlQName key) [] [CRef val] Nothing


-- |
-- Create an XML Element from a String, a list of attributes and a list of contents.
-- If the input is a tuple of strings, create an Element with the first as a tag and the second as content.
-- Otherwise, return the Element val as Elem val (to create Content).
xmlElement :: String -> [(String, String)] -> [Either (String, String) Element] -> Element
xmlElement name attrs contLst = Element (xmlQName name) attributes contents Nothing
    where
        attributes      = xmlAttr   <$> attrs
        contents        = parseList <$> contLst
        parseList conts = case conts of
            Left  tuple   -> xmlContent tuple
            Right element -> Elem element


-- | Create a QName from a String.
xmlQName :: String -> QName
xmlQName str = QName str mempty mempty
