------------------------------------------------------------------------------
-- |
-- Module      :  PCG.Software.Credits
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module PCG.Software.Credits
  ( authorsList
  , fundingList
  ) where

import qualified Control.Foldl              as L
import           Control.Monad
import           Data.Foldable
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Text                  (Text)
import           Data.Text.IO               (readFile)
import           Instances.TH.Lift          ()
import           Language.Haskell.TH        hiding (Inline)
import           Language.Haskell.TH.Syntax hiding (Inline)
import           Prelude                    hiding (readFile)
import           Text.MMark
import           Text.MMark.Extension
import qualified Text.URI                   as URI


-- |
-- List of authors who have contributed to PCG.
authorsList :: ExpQ
authorsList = lift =<< getAuthorFileData
  where
    getAuthorFileData = runIO $ getAuthorLines <$> readFile "doc/AUTHORS.md"

    getAuthorLines :: Text -> [Text]
    getAuthorLines = fmap fst . fromMarkdown processMarkdown


-- |
-- List of funding sources which have contributed to PCG.
fundingList :: ExpQ
fundingList = lift =<< getFundingFileData
  where
    getFundingFileData = runIO $ getFundingLines <$> readFile "doc/FUNDING.md"

    getFundingLines :: Text -> [(Text, Maybe Text)]
    getFundingLines = fromMarkdown processMarkdown


processMarkdown :: MMark -> [(Text, Maybe Text)]
processMarkdown = (`runScanner` L.foldMap g f)
  where
    f :: [Block (NonEmpty Inline)] -> [(Text, Maybe Text)]
    f = foldMap renderItem

    g :: Block a -> [Block a]
    g = getListBlocks


getListBlocks :: Block a -> [Block a]
getListBlocks = fold . foldMap toList . toList . getListMay


getListMay :: Block a -> Maybe (NonEmpty [Block a])
getListMay (OrderedList _ xs) = Just xs
getListMay (UnorderedList xs) = Just xs
getListMay _                  = Nothing


fromMarkdown :: Monoid a => (MMark -> a) -> Text -> a
fromMarkdown f = foldMap f . parse ""


renderItem :: Block (NonEmpty Inline) -> [(Text, Maybe Text)]
renderItem   (CodeBlock _ val) = [(val, Nothing)]
renderItem   (Naked       val) = foldMap renderInline val
renderItem   (Paragraph   val) = foldMap renderInline val
renderItem   _                 = []


renderInline :: Inline -> [(Text, Maybe Text)]
renderInline (Plain       txt) = [(txt, Nothing)]
renderInline (Emphasis    val) = [(asPlainText val, Nothing)]
renderInline (Strong      val) = [(asPlainText val, Nothing)]
renderInline (Strikeout   val) = [(asPlainText val, Nothing)]
renderInline (Subscript   val) = [(asPlainText val, Nothing)]
renderInline (Superscript val) = [(asPlainText val, Nothing)]
renderInline (CodeSpan    txt) = [(txt, Nothing)]
renderInline (Link val uri _ ) = [(asPlainText val, Just $ URI.render uri)]
renderInline  _                = []
