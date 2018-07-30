{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}

module Main (main) where

import Data.List.NonEmpty hiding (unfoldr)
import Data.Maybe
import Data.Semigroup
import Data.Void
import File.Format.Newick
import System.Environment  -- for command-line arguments.
import Text.Megaparsec


data  BinaryTree a
    = Leaf a
    | Branch (BinaryTree a) (BinaryTree a)
    deriving (Eq, Foldable, Functor, Traversable, Ord, Show)


-- |
-- Get line from file on argv
-- For each line:
--     Call Newick parser
-- For each tree:
--     Call resolveNode
-- Output results
main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        []                -> putStrLn "No arguments supplied. Need a Newick file name."
        inputfileName : _ -> do
            inputStream  <- readFile inputfileName
            case parse' newickStreamParser inputfileName inputStream of
                Left  errMsg -> putStrLn $ parseErrorPretty' inputfileName errMsg
                Right forest -> nicelyPrintAllResolutions $ renderAllResolutions <$> forest
    where
        parse' :: Parsec Void s a -> String -> s -> Either (ParseError (Token s) Void) a
        parse' = parse
        appendSemicolon = (<> ";")
        getLabel = fromJust . newickLabel
        renderBinaryTree = appendSemicolon . renderNewickString . fmap getLabel
        renderAllResolutions = fmap (foldMap (<>"\n") . fmap renderBinaryTree . unfoldr descendants)
        nicelyPrintAllResolutions = mapM_ putStrLn . sconcat


-- |
-- Take a 'NewickNode' and map over its descendents to render the entire string in Newick format.
renderNewickString :: BinaryTree String -> String
renderNewickString (Leaf x) = x
renderNewickString (Branch lhs rhs) = mconcat 
    [ "("
    , renderNewickString lhs
    , ", "
    , renderNewickString rhs
    , ")" 
    ]


unfoldr
  :: (a -> [a]) -- ^ Generating function (extract children)
  -> a          -- ^ Seed value (root node)
  -> NonEmpty (BinaryTree a)
unfoldr kids = resolveNode
  where
    -- |
    -- Given a node of a tree, return a non-empty set of all the unique
    -- ways that node's subtrees can be resolved as binary trees.
    resolveNode node =
        case kids node of
          []   -> pure $ Leaf node
          [x]  -> resolveNode x
          x:xs -> do
              -- What are all the unique combinations of ways
              -- I can split my children into two non-empty sets.
              (lhs, rhs)  <- splittings $ x:|xs
              -- Construct all possible binary trees where
              -- the subtrees in each partition are the leaf set
              lhsJoined   <- joinSubtrees lhs
              rhsJoined   <- joinSubtrees rhs
              -- Recursively resolve the nodes of the subtrees
              lhsResolved <- traverse resolveNode lhsJoined
              rhsResolved <- traverse resolveNode rhsJoined
              -- Splice partitions together
              pure $ Branch (collapse lhsResolved) (collapse rhsResolved)


-- |
-- Given a binary tree with leaves of binary trees, construct binary tree.
collapse :: BinaryTree (BinaryTree a) -> BinaryTree a
collapse (Leaf x) = x
collapse (Branch lhs rhs) = Branch (collapse lhs) $ collapse rhs


-- |
-- Given a non-empty set of subtrees, this function returns a list of
-- all the ways to join the subtrees together into a single tree.
--
-- Note, that the newly created internal verticies are decorated with
-- the 'mempty' value of the monoidal decoration type.
joinSubtrees :: NonEmpty a -> NonEmpty (BinaryTree a)
joinSubtrees (x:|[])  = pure $ Leaf x
joinSubtrees subtrees = do
    (lhs, rhs)  <- splittings subtrees
    lhsResolved <- joinSubtrees lhs
    rhsResolved <- joinSubtrees rhs
    pure $ Branch lhsResolved rhsResolved


-- |
-- Given a non-empty set of elements, returns all the unique ways
-- to split the input set into  two non-empty sets
splittings :: NonEmpty a -> NonEmpty (NonEmpty a, NonEmpty a)
splittings (_:|[])   = error "Singleton list of children supplied"
splittings (x:|[y])  = pure (pure x, pure y)
splittings (x:|y:ys) = pure (pure x, y:|ys) <> do
    (lhs, rhs) <- splittings $ y:|ys
    (pure x <> lhs, rhs) :| [(lhs, pure x <> rhs)]
