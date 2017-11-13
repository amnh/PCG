---------------------------------------------------------------------------
-- |
-- Module      :  Text.HTML.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Output an HTML.Element from various input types.
-- Includes helper fns, all of which are exported.
--
-----------------------------------------------------------------------------

module Text.HTML.Custom
  ( ToHTML (..)
  , collapseElemList
  , hTMLAttr
  , hTMLContent
  , hTMLElement
  , hTMLQName
  ) where


import Data.Foldable
import Text.HTML.Class
import Text.Blaze.Html


-- |
-- Take in a list of ToHTML items and return a single Element with HTML'ed items as substructure.
-- Used in ToHTML instances when there are sequences of data.
--
-- Wanted to make it take any Traversable sequence, but realized that wouldn't guarantee 'toList' was available.
-- TODO: find a way around that?
collapseElemList :: (Foldable f, ToHTML a) => String -> [Attr] -> f a -> Element
collapseElemList name attrs lst = Element (HTMLQName name) attrs contents Nothing
    where
        contents = Elem . toHTML <$> toList lst


-- |
-- Create an HTML Attr, which is a key value pair (HTMLQName, String).
hTMLAttr :: (String, String) -> Attr
hTMLAttr (name, val) = Attr (HTMLQName name) val


-- |
-- Create HTML Contents, which for our use is always an Element.
hTMLContent :: (String, String) -> Content
hTMLContent (key, val) = Elem $ Element (HTMLQName key) [] [CRef val] Nothing


-- |
-- Create an HTML Element from a String, a list of attributes and a list of contents.
-- If the input is a tuple of strings, create an Element with the first as a tag and the second as content.
-- Otherwise, return the Element val as Elem val (to create Content).
hTMLElement :: String -> [(String, String)] -> [Either (String, String) Element] -> Element
hTMLElement name attrs contLst = Element (HTMLQName name) attributes contents Nothing
    where
        attributes      = HTMLAttr   <$> attrs
        contents        = parseList <$> contLst
        parseList conts = case conts of
            Left  tuple   -> HTMLContent tuple
            Right element -> Elem element


-- | Create a QName from a String.
hTMLQName :: String -> QName
hTMLQName str = QName str mempty mempty


-- -- | Coerce an input of (String, Either String Element) into Content
-- parseElemTupleToContent :: (String, Either String Element) -> Content
-- parseElemTupleToContent  (tag, Left str    ) = HTMLContent (tag, str)
-- parseElemTupleToContent  (_  , Right inElem) = Elem inElem
