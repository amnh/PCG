module File.Format.Newick.Internal
  ( NewickForest
  , NewickNode(..)
  , isLeaf
  , newickNode
  ) where

import Data.Maybe (isJust,isNothing)

{----
  - The Newick file format was developed by an informal committee meeting at
  - Newick's seafood restaurant. The grammar definition of the Newick format
  - was never formally specified, but Gary Olsen's interpretation of the 
  - original newick format has been documented here: 
  - http://evolution.genetics.washington.edu/phylip/newick_doc.html
  - 
  - After over two decades of informal usage, the Extended Newick file format
  - was proposed in a BCM publication which allowed node labels to be non-
  - unique and merged to a single node with shared ancestors and descendants.
  - This allowed for easy manual annotating of phylogentic trees.
  -
  - Another half decade later, the Forest Extended Newick was proposed by
  - Professor Wheeler to model collections of disjoint phylogenetic trees.
  - This new format allowed grouping many Extended Newick trees into a 
  - forest to be analyzed collectively.
  -
  - This parser correctly parses both Newick file formats, and the super set
  - Extended Newick filed format.
  -}

type NewickForest = [NewickNode]

data NewickNode
   = NewickNode
   { descendants  :: [NewickNode] --leaf nodes are empty lists
   , newickLabel  :: Maybe String --leaf nodes will always be Just
   , branchLength :: Maybe Double
   } deriving (Eq,Ord)

instance Show NewickNode where
  show (NewickNode d n b) = name ++ len ++ " " ++ show d 
    where
      name = maybe "Node" show n
      len  = maybe "" (\x -> ':' : show x) b

newickNode :: [NewickNode] -> Maybe String -> Maybe Double -> Maybe NewickNode
newickNode nodes label length'
  | null nodes && isNothing label = Nothing
  | otherwise = Just $ NewickNode nodes label length'

isLeaf :: NewickNode -> Bool
isLeaf node = (null . descendants) node && (isJust . newickLabel) node
