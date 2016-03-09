module Test.Custom.Graph where

import Bio.Phylogeny.Graph
import Test.Tasty.HUnit
import Text.Megaparsec (Parsec,parse)

-- TODO: determine if this should be included in this module, adds very many dependacies just to import `Graph`
convertEquals :: (Eq a, Show a) => Parsec String a -> String -> Graph -> (a -> Graph) -> Assertion
convertEquals parser str expected converter =
  case result of
    Left  x -> assertFailure $ show x
    Right x -> assertEqual ("Parse result of :" ++ show str) expected x
  where
    result = converter <$> parse parser "" str
