{-# LANGUAGE LambdaCase #-}

module NetworkEdges where

import           Data.Foldable
import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.IO                 as TL
import           System.FilePath.Posix             ((<.>))


makeDotFile :: (Network, FilePath) -> IO ()
makeDotFile ((nodes, edges), filename) = do
    let
      dotGraph = G.graphElemsToDot networkGraphParameters nodes edges :: G.DotGraph String
      dotText  =  G.printDotGraph dotGraph :: TL.Text
    TL.writeFile (filename <.> "dot") dotText


data EdgeLabel =
    ExistingEdgeLabel
  | NetEdgeLabel
  | NewNetEdgeLabel
  | NewEdgeLabel
  | CandNetEdgeLabel


data NodeLabel =
    ExistingNodeLabel
  | ContextualNodeLabel
  | NewNodeLabel

type Node = (String, NodeLabel)
type Edge = (String, String, EdgeLabel)

existingE :: (String, String) -> (String, String, EdgeLabel)
existingE (s,t)  = (s, t, ExistingEdgeLabel)

newE :: (String, String) -> (String, String, EdgeLabel)
newE (s,t)  = (s, t, NewEdgeLabel)

newNetE :: (String, String) -> (String, String, EdgeLabel)
newNetE (s,t)  = (s, t, NewNetEdgeLabel)

candidateE :: (String, String) -> (String, String, EdgeLabel)
candidateE (s,t) = (s, t, CandNetEdgeLabel)

netE :: (String, String) -> (String, String, EdgeLabel)
netE (s,t) = (s, t, NetEdgeLabel)

existingN :: String -> (String, NodeLabel)
existingN n = (n, ExistingNodeLabel)

contextualN :: String -> (String, NodeLabel)
contextualN n = (n, ContextualNodeLabel)

newN :: String -> (String, NodeLabel)
newN n = (n, NewNodeLabel)


type Network = ([Node], [Edge])

networkGraphParameters
 :: G.GraphvizParams
       String          -- vertex type
       NodeLabel       -- vertex label type
       EdgeLabel       -- edge label type
       ()              -- cluster type
       NodeLabel       -- cluster label type
networkGraphParameters = G.defaultParams {
      G.fmtNode  = \case
        (_, ExistingNodeLabel  ) -> colorAttribute  black   <> point
        (_, ContextualNodeLabel) -> colorAttribute  teal    <> point
        (_, NewNodeLabel       ) -> colorAttribute  red     <> point

    , G.fmtEdge = \case
        (_, _, ExistingEdgeLabel)  -> colorAttribute black <> order
        (_, _, NewEdgeLabel     )  -> colorAttribute teal <> noArrow
        (_, _, NewNetEdgeLabel     )  -> colorAttribute teal <> noArrow <> dotted
        (_, _, CandNetEdgeLabel )  -> colorAttribute red  <> dotted <> emptyArrowHead
        (_, _, NetEdgeLabel     )  -> colorAttribute black <> dotted <> noArrow
        }
  where
    colorAttribute color = [ G.Color $ G.toColorList [ color ] ]
    black = G.RGB 0 0 0
    red  = G.RGB 204 2 2
    teal = G.RGB 0 102 102
    dotted = [G.Style [G.SItem G.Dashed mempty]]
    noArrow = [G.ArrowHead $ G.AType [(G.noMods, G.NoArrow)]]
    emptyArrowHead = [G.ArrowHead $ G.AType [(G.noMods, G.Vee)]]
    point = [G.Shape G.PointShape]
    order = [G.Ordering G.OutEdges]
    


--------------------------------------
--            Examples              --
--------------------------------------

networks :: [(Network, FilePath)]
networks = [ ( baseNetwork
             , "base-network"
             )
           , ( hasE2IncidentNetworkNode
             , "hasE2IncidentNetworkNode"
             )
           , ( e2AncestralEdgeNetwork
             , "e2AncestralEdgeNetwork"
             )
           , ( e1HasE2SrcAncestralNodeNetwork
             , "e1HasE2SrcDescendantNodeNetwork"
             )
           , ( e2NetworkEdgeComplementNodeAncestralToE1
             , "e2NetworkEdgeComplementNodeAncestralToE1"
             )
           ]


--  Base Network:
--
--                         root
--                       /     \
--                      a       b
--                    /  \     /  \
--                   c    \   /    e
--                          d     /  \
--                          |    g    \
--                          f   / \    \
--                         / \  k  l    \
--                        i   j          h
--                                      / \
--                                     m   n
--                                    / \ / \
--                                   p   o   q
--                                       |
--                                       r
--
--

baseNetwork :: Network
baseNetwork =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = existingN <$> ["root", "a", "b" , "c", "d", "e", "f"
                          , "g", "h", "i", "j", "k", "l", "m", "n"
                          , "o", "p", "q", "r"
                          ]

    edges :: [Edge]
    edges = fold
            [ existingE <$>
                [ ("root", "a"), ("root", "b")
                , ("a", "c"), ("b", "e")
                , ("d", "f"), ("e", "g"), ("e", "h")
                , ("f", "i"), ("f", "j")
                , ("g", "k"), ("g", "l")
                , ("h", "m"), ("h", "n")
                , ("m", "p"), ("n", "q")
                , ("o", "r")
                ]
            , netE <$>
                [ ("a" ,"d"), ("b", "d")
                , ("m", "o"), ("n", "o")
                ]
            ]


-- Edge added: (b,e) -> (g, k)
-- Problem: The edge (b,e) is ancestral to (g,k) and so this is not allowed
e2AncestralEdgeNetwork :: Network
e2AncestralEdgeNetwork =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ existingN   <$>  ["root", "a", "c", "d", "f" , "h", "i", "j"
                               , "l", "m", "n" , "o", "p", "q", "r"
                               ]
            , newN        <$> ["newSrc", "newTgt"]
            , contextualN <$> ["b (src1)", "g (src2)", "e (tgt1)", "k (tgt2)"]
            ]

    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a"), ("root", "b (src1)")
                  , ("a", "c")
                  , ("d", "f"), ("e (tgt1)", "g (src2)"), ("e (tgt1)", "h")
                  , ("f", "i"), ("f", "j")
                  , ("g (src2)", "l")
                  , ("h", "m"), ("h", "n")
                  , ("m", "p"), ("n", "q")
                  , ("o", "r")
                  ]
            , netE <$>
                [ ("a" ,"d"), ("b (src1)", "d")
                , ("m", "o"), ("n", "o")
                ]
            , candidateE <$> [("newSrc", "newTgt")]
            , newE       <$> [ ("b (src1)"  , "newSrc")
                             , ("newSrc", "e (tgt1)"  )
                             , ("g (src2)"  , "newTgt")
                             , ("newTgt", "k (tgt2)"  )
                             ]
            ]


-- Edge added: (h,m) -> (e, g)
-- Problem: The edge (h,m) has e as an ancestral network node and this would
-- go against the implied ordering.
e1HasE2SrcAncestralNodeNetwork :: Network
e1HasE2SrcAncestralNodeNetwork =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ existingN   <$>  ["root", "a", "b", "c", "d", "f", "i", "j", "k"
                               , "l", "n" , "o", "p", "q", "r"
                               ]
            , newN        <$> ["newSrc", "newTgt"]
            , contextualN <$> ["h (src1)", "e (src2)", "m (tgt1)", "g (tgt2)"]
            ]

    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a"), ("root", "b")
                  , ("a", "c"), ("b", "e (src2)")
                  , ("d", "f"), ("e (src2)", "h (src1)")
                  , ("f", "i"), ("f", "j")
                  , ("g (tgt2)", "k"), ("g (tgt2)", "l")
                  , ("h (src1)", "n")
                  , ("m (tgt1)", "p"), ("n", "q")
                  , ("o", "r")
                  ]
            ,  netE <$>
                [ ("a" ,"d"), ("b", "d")
                , ("m (tgt1)", "o"), ("n", "o")
                ]
            , candidateE <$> [ ("newSrc", "newTgt")]
            , newE       <$> [ ("h (src1)"  , "newSrc")
                             , ("newSrc", "m (tgt1)"  )
                             , ("e (src2)"  , "newTgt")
                             , ("newTgt", "g (tgt2)"  )
                             ]
            ]
            
-- Edge added: (g, l) -> (m, o)
-- Problem: We cannot have a new edge into an edge
-- adjacent to a network node as leads to having a display tree
-- with a new leaf.
hasE2IncidentNetworkNode :: Network
hasE2IncidentNetworkNode =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ existingN   <$>  ["root", "a", "b", "c", "d", "e", "f", "h", "i", "j"
                               , "k", "n", "p", "q" ,"r"
                               ]
            , newN        <$> ["newSrc", "newTgt"]
            , contextualN <$> ["g (src1)", "m (src2)", "l (tgt1)", "o (tgt2)"]
            ]

    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a"), ("root", "b")
                  , ("a", "c"), ("b", "e")
                  , ("d", "f"), ("e", "g (src1)"), ("e", "h")
                  , ("f", "i"), ("f", "j")
                  , ("g (src1)", "k")
                  , ("h", "n"), ("h", "m (src2)")
                  , ("m (src2)", "p"), ("n", "q")
                  , ("o (tgt2)", "r")
                  ]
            , netE <$>
                [ ("a" ,"d"), ("b", "d")
                , ("n", "o (tgt2)")
                ]
            , candidateE <$> [ ("newSrc", "newTgt")]
            , newE       <$> [ ("g (src1)"  , "newSrc")
                             , ("newSrc", "l (tgt1)"  )
                             ]
            , newNetE   <$>  [ ("m (src2)"  , "newTgt")
                             , ("newTgt", "o (tgt2)"  )
                             ]
            ]




-- Edge added: (b,e) -> (g, k)
-- Problem: The edge (b,e) is ancestral to (g,k) and so this is not allowed
src2NetworkPairAncestralToE1Network :: Network
src2NetworkPairAncestralToE1Network =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ existingN   <$>  ["root", "d", "f", "g"
                               , "h", "i", "j" , "k", "l", "m", "n" , "o"
                               , "p", "q", "r"
                               ]
            , newN        <$> ["newSrc", "newTgt"]
            , contextualN <$> ["a (src1)", "b (src2)", "c (tgt1)", "e (tgt2)"]
            ]

    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a (src1)"), ("root", "b")
                  , ("a (src1)", "c"), ("a (src1)" ,"d"), ("b", "d"), ("b", "e")
                  , ("d", "f"), ("e", "g"), ("e", "h")
                  , ("f", "i"), ("f", "j")
                  , ("g", "k"), ("g", "l")
                  , ("h", "m"), ("h", "n")
                  , ("m", "o"), ("n", "o"), ("m", "p"), ("n", "q")
                  , ("o", "r")
                  ]
            , candidateE <$> [ ("newSrc", "newTgt")]
            , newE       <$> [ (" (src1)"  , "newSrc")
                             , ("newSrc", " (tgt1)"  )
                             , (" (src2)"  , "newTgt")
                             , ("newTgt", " (tgt2)"  )
                             ]
            ]


-- Edge added: (g,k) -> (a, c)
-- Problem: a is a network parent node with partner b which is ancestral
-- to g. As such adding this edge would lead to an inconsistent history.
e2NetworkEdgeComplementNodeAncestralToE1 :: Network
e2NetworkEdgeComplementNodeAncestralToE1 =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ existingN <$> ["root","b", "d", "e", "f"
                            , "h", "i", "j", "l", "m", "n"
                            , "o", "p", "q", "r"
                            ]
            
            , newN        <$> ["newSrc", "newTgt"]
            , contextualN <$> ["g (src1)", "a (src2)", "k (tgt1)", "c (tgt2)"]
            ]


    edges :: [Edge]
    edges = fold
            [ existingE <$>
                [ ("root", "a (src2)"), ("root", "b")
                , ("b", "e")
                , ("d", "f"), ("e", "g (src1)"), ("e", "h")
                , ("f", "i"), ("f", "j")
                , ("g (src1)", "l")
                , ("h", "m"), ("h", "n")
                , ("m", "p"), ("n", "q")
                , ("o", "r")
                ]
            , netE <$>
                [ ("a (src2)" ,"d"), ("b", "d")
                , ("m", "o"), ("n", "o")
                ]
            , candidateE <$> [("newSrc", "newTgt")]
            , newE       <$> [ ("g (src1)"  , "newSrc")
                             , ("newSrc", "k (tgt1)"  )
                             , ("a (src2)"  , "newTgt")
                             , ("newTgt", "c (tgt2)"  )
                             ]
            ]


