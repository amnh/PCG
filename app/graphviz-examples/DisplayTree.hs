{-# LANGUAGE LambdaCase #-}

module DisplayTree where

import           Data.Foldable
import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.IO                 as TL
import           System.FilePath.Posix             ((<.>))


-- |
-- Make a dot file from a 'Network'.
makeDotFile :: (Network, FilePath) -> IO ()
makeDotFile ((nodes, edges), filename) = do
    let
      dotGraph = G.graphElemsToDot networkGraphParameters nodes edges :: G.DotGraph String
      dotText  =  G.printDotGraph dotGraph :: TL.Text
    TL.writeFile (filename <.> "dot") dotText


-- |
-- Labels we use for types of edges in a display tree diagram.
data DisplayEdge =
    Edge
  | NetEdge
  | KeptEdge
  | DeletedEdge

-- |
-- Labels we use for types of nodes in a display tree diagram.
data NodeLabel =
    RealNode
  | IgnoredSubtree
  | BadNode

-- |
-- A Node is a NodeLabel type and identifier.
type Node = (String, NodeLabel)

-- |
-- An edge is a DisplayEdge type and identifier.
type Edge = (String, String, DisplayEdge)

-- |
-- A Network consists of a pair of lists of 'Node's and 'Edge's
type Network = ([Node], [Edge])

-- |
-- Smart constructor for edge.
edgeE :: (String, String) -> (String, String, DisplayEdge)
edgeE (s,t)  = (s, t, Edge)

-- |
-- Smart constructor for edge.
keptE :: (String, String) -> (String, String, DisplayEdge)
keptE (s,t)  = (s, t, KeptEdge)

-- |
-- Smart constructor for edge.
netE :: (String, String) -> (String, String, DisplayEdge)
netE (s,t)  = (s, t, NetEdge)

-- |
-- Smart constructor for edge.
deletedE :: (String, String) -> (String, String, DisplayEdge)
deletedE (s,t) = (s, t, DeletedEdge)

-- |
-- Smart constructor for node.
realN :: String -> (String, NodeLabel)
realN n = (n, RealNode)

-- |
-- Smart constructor for node.
ignoredN :: String -> (String, NodeLabel)
ignoredN n = (n, IgnoredSubtree)

-- |
-- Smart constructor for node.
badN :: String -> (String, NodeLabel)
badN n = (n, BadNode)




-- |
-- Parameters for how to render a display tree for a network.
networkGraphParameters
 :: G.GraphvizParams
       String          -- vertex type
       NodeLabel       -- vertex label type
       DisplayEdge     -- edge label type
       ()              -- cluster type
       NodeLabel       -- cluster label type
networkGraphParameters = G.defaultParams {
    G.fmtNode = \case
        (_, RealNode      ) -> colorAttribute  black <> point
        (_, IgnoredSubtree) -> colorAttribute  black <> box
        (_, BadNode       ) -> colorAttribute  red   <> point
    ,
    G.fmtEdge = \case
        (_, _, Edge       )  -> colorAttribute black
        (_, _, KeptEdge   )  -> colorAttribute green
        (_, _, DeletedEdge)  -> noArrow <> dotted <> colorAttribute blue
        (_, _, NetEdge    )  -> colorAttribute black <> dotted <> noArrow
        }
  where
    colorAttribute color = [ G.Color $ G.toColorList [color] ]
    black = G.RGB 0 0 0
    blue   = G.RGB 30 144 255
    green = G.RGB 0 100 0
    red  = G.RGB 204 2 2
    dotted = [G.Style [G.SItem G.Dashed mempty]]
    noArrow = [G.ArrowHead $ G.AType [(G.noMods, G.NoArrow)]]
    point = [G.Shape G.PointShape]
    box   = [G.Shape G.BoxShape] <> dotted <> [G.Area 0.5]



--------------------------------------
--            Examples              --
--------------------------------------

-- |
-- Collection of examples of display trees.
displayTrees :: [(Network, FilePath)]
displayTrees = [ (doubleNetworkNodes  , "disallowed-display-network1")
               , (doubleNetworkNodesDT, "disallowed-display-tree1"   )
               ]

-- Inconsistent display tree
--
--          [...]
--            |
--            o
--           / \
--          /   \
--         o     o
--        / \    /\
--       /   \  / [..]
--      o     o
--     / \   /
--  [..]  \ /
--         o
--         |
--        [..]


--          [...]
--            |
--            o
--           / \
--          /   \
--         o     o
--        / \     \
--       /   \    [..]
--      o     o
--     / \
--  [..]  \
--         o
--         |
--        [..]

-- |
-- Example of disallowed phylogenetic network.
doubleNetworkNodes :: Network
doubleNetworkNodes =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ realN    <$>  ["a", "b", "c", "d", "e", "f"]
            , ignoredN <$> ["sub-tree1", "sub-tree2", "sub-tree3", "sub-tree4"]

            ]


    edges :: [Edge]
    edges = fold
            [ edgeE    <$> [ ("sub-tree1", "a"), ("a", "b"), ("a", "c")
                           , ("b", "d"), ("c", "sub-tree2")
                           , ("d", "sub-tree3")
                           , ("f", "sub-tree4")
                           ]
            , netE     <$> [  ("b", "e") ,  ("c", "e")
                           ,  ("d", "f") ,  ("e", "f")
                           ]
            , keptE    <$> []
            , deletedE <$> []
            ]


-- |
-- Display tree associated to 'doubleNetworkNodes'.
doubleNetworkNodesDT :: Network
doubleNetworkNodesDT =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ realN    <$>  ["a", "b", "c", "d" , "e", "f"]
            , ignoredN <$> ["sub-tree1", "sub-tree2", "sub-tree3", "sub-tree4"]
            , badN     <$> ["e"]
            ]


    edges :: [Edge]
    edges = fold
            [ edgeE    <$> [ ("sub-tree1", "a"), ("a", "b"), ("a", "c")
                           , ("b", "d"), ("c", "sub-tree2")
                           , ("d", "sub-tree3")
                           , ("f", "sub-tree4")
                           ]
            , keptE    <$> [ ("b", "e"), ("d", "f")]
            , deletedE <$> [ ("c", "e"), ("e", "f") ]
            ]



