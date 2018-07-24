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


{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}


module Text.XML.Class where
    -- ( ToXML (..)
    -- , Content (CRef)
    -- , collapseElemList
    -- , xmlAttr
    -- , xmlContent
    -- , xmlElement
    -- , xmlQName
    -- ) where


import Data.Foldable
import Data.Key
-- import Text.XML.Custom
import Text.XML.Light.Types


-- | (✔)
class ToXML a where

    toXML :: a -> Element


-- | (✔)
instance (ToXML a) => ToXML (Maybe a) where

    toXML input = result -- Element (QName "Maybe_data" Nothing Nothing) [] [content] Nothing
        where
            result = case input of
                          Nothing  -> Element (xmlQName "No_data") [] [] Nothing
                          Just val -> toXML val


-- | (✔)
instance (ToXML a, ToXML b) => ToXML (a, b) where

    toXML (a, b) = Element (QName "Tuple" Nothing Nothing) [] [Elem $ toXML a, Elem $ toXML b] Nothing


-- | (✔)
instance ToXML [Char] where

    toXML val = Element (QName "Text_value" Nothing Nothing) [] [CRef val] Nothing


-- | (✔)
instance {-# OVERLAPPABLE #-} (Foldable f, ToXML a) => ToXML (f a) where

    toXML lst = Element name attrs contents Nothing
       where
           name     = QName "List" Nothing Nothing
           attrs    = []
           contents = Elem . toXML <$> toList lst


instance ToXML Int where

    toXML i = Element name [] contents Nothing
       where
         name     = QName "Int" Nothing Nothing
         attrs    = []
         contents = [CRef $ show i]

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
