module Resolve where

import Data.List.NonEmpty
import Data.Maybe
import Data.Semigroup
-- import Data.Tree
import Data.Void
import File.Format.Newick
import System.Environment  -- for command-line arguments.
import Text.Megaparsec


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
                -- Right forest -> print $ fmap resolveNode <$> forest
                -- Right forest -> mapM_ putStrLn . sconcat $ fmap (renderNewickForest . resolveNode) <$> forest
                Right forest -> mapM_ putStrLn . sconcat $ fmap (unlines . toList . fmap (appendSemicolon . renderNewickString) . resolveNode) <$> forest
    where
        parse' :: Parsec Void s a -> String -> s -> Either (ParseError (Token s) Void) a
        parse' = parse
        appendSemicolon = (<> ";")


-- |
-- Take a 'NewickNode' and map over its descendents to render the entire string in Newick format.
renderNewickString :: NewickNode -> String
renderNewickString inNode =
    case descendants inNode of
        []    -> fromJust $ newickLabel inNode
        [x]   -> mconcat [ "(", renderNewickString x, ")" ]
        x:y:_ -> mconcat [ "(", renderNewickString x, ", ", renderNewickString y, ")" ]


-- |
-- Given a node of a tree, return a non-empty set of all the unique
-- ways that node's subtrees can be resolved as binary trees.
resolveNode :: NewickNode -> NewickForest
resolveNode node =
    case descendants node of
      []   -> pure node
      [x]  -> resolveNode x >>= (\y -> resolveNode $ makeNode [y])
      x:xs -> do
          (lhs, rhs)  <- splittings $ x:|xs
          lhsJoined   <- joinSubtrees lhs
          rhsJoined   <- joinSubtrees rhs
          lhsResolved <- resolveNode lhsJoined
          rhsResolved <- resolveNode rhsJoined
          pure $ makeNode [lhsResolved, rhsResolved]


-- |
-- Given a non-empty set of elements, returns all the unique ways
-- to split the input set into  two non-empty sets
splittings :: NonEmpty a -> NonEmpty (NonEmpty a, NonEmpty a)
splittings (x:|[])   = error "Singleton list of chilren supplied"
splittings (x:|[y])  = pure (pure x, pure y)
splittings (x:|y:ys) = pure (pure x, y:|ys) <> do
    e@(lhs, rhs) <- splittings $ y:|ys
    (pure x <> lhs, rhs) :| [(lhs, pure x <> rhs)]


-- |
-- Given a non-empty set of subtrees, this function returns a list of
-- all the ways to join the subtrees together into a single tree.
--
-- Note, that the newly created internal verticies are decorated with
-- the 'mempty' value of the monoidal decoration type.
joinSubtrees :: NewickForest -> NewickForest
joinSubtrees e@(x:|[]) = e
joinSubtrees subtrees = do
    (lhs, rhs)  <- splittings subtrees
    lhsResolved <- joinSubtrees lhs
    rhsResolved <- joinSubtrees rhs
    resolveNode $ makeNode [lhsResolved, rhsResolved]


-- |
--
makeNode :: [NewickNode] -> NewickNode
makeNode children = fromJust $ newickNode children Nothing Nothing
