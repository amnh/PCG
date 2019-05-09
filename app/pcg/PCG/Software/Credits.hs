module PCG.Software.Credits
  ( authorsList
  , fundingList
  ) where

import qualified Control.Foldl              as L
import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.Text                  (Text, unpack)
import           Data.Text.IO               (readFile)
import           Instances.TH.Lift          () -- Exposes instance (Lift Text)
import           Language.Haskell.TH        hiding (Inline)
import           Language.Haskell.TH.Syntax hiding (Inline)
import           Prelude                    hiding (readFile)
import           Text.MMark
import           Text.MMark.Extension
import qualified Text.URI                   as URI


authorsList :: ExpQ
authorsList = lift =<< getAuthorFileData
  where
    getAuthorFileData = runIO $ getAuthorLines <$> readFile "AUTHORS.md"

    getAuthorLines :: Text -> [Text]
    getAuthorLines = fmap fst . fromMarkdown processMarkdown


fundingList :: ExpQ
fundingList = lift =<< getFundingFileData
  where
    getFundingFileData = runIO $ getFundingLines <$> readFile "FUNDING.md"

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
