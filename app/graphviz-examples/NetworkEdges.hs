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

candidateE :: (String, String) -> (String, String, EdgeLabel)
candidateE (s,t) = (s, t, CandNetEdgeLabel)

existingN :: String -> (String, NodeLabel)
existingN n = (n, ExistingNodeLabel)

contextualN :: String -> (String, NodeLabel)
contextualN n = (n, ContextualNodeLabel)

newN :: String -> (String, NodeLabel)
newN n = (n, NewNodeLabel)


type Network = ([Node], [Edge])

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
    edges = existingE <$>
              [ ("root", "a"), ("root", "b")
              , ("a", "c"), ("a" ,"d"), ("b", "d"), ("b", "e")
              , ("d", "f"), ("e", "g"), ("e", "h")
              , ("f", "i"), ("f", "j")
              , ("g", "k"), ("g", "l")
              , ("h", "m"), ("h", "n")
              , ("m", "o"), ("n", "o"), ("m", "p"), ("n", "q")
              , ("o", "r")
              ]


-- Edge added: (b,e) -> (g, k)
-- Problem: The edge (b,e) is ancestral to (g,k) and so this is not allowed
e2AncestralEdgeNetwork :: Network
e2AncestralEdgeNetwork =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ existingN   <$>  ["root", "a", "c", "d", "f" , "h", "j"
                               , "l", "m", "n" , "o", "p", "q", "r"
                               ]
            , newN        <$> ["newSrc", "newTgt"]
            , contextualN <$> ["b (src1)", "g (src2)", "e (tgt1)", "k (tgt2)"]
            ]

    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a"), ("root", "b (src1)")
                  , ("a", "c"), ("a" ,"d"), ("b (src1)", "d")
                  , ("d", "f"), ("e (tgt1)", "g (src2)"), ("e (tgt1)", "h")
                  , ("f", "i"), ("f", "j")
                  , ("g (src2)", "l")
                  , ("h", "m"), ("h", "n")
                  , ("m", "o"), ("n", "o"), ("m", "p"), ("n", "q")
                  , ("o", "r")
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
-- go against the implied ordering
e1HasE2SrcAncestralNodeNetwork :: Network
e1HasE2SrcAncestralNodeNetwork =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ existingN   <$>  ["root", "a", "c", "d", "f", "i", "j"
                               , "l", "n" , "o", "p", "q", "r"
                               ]
            , newN        <$> ["newSrc", "newTgt"]
            , contextualN <$> ["h (src1)", "e (src2)", "m (tgt1)", "g (tgt2)"]
            ]

    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a"), ("root", "b")
                  , ("a", "c"), ("a" ,"d"), ("b", "d"), ("b", "e (src2)")
                  , ("d", "f"), ("e (src2)", "h (src1)")
                  , ("f", "i"), ("f", "j")
                  , ("g (tgt2)", "k"), ("g (tgt2)", "l")
                  , ("h (src1)", "n")
                  , ("m (tgt1)", "o"), ("n", "o"), ("m (tgt1)", "p"), ("n", "q")
                  , ("o", "r")
                  ]
            , candidateE <$> [("newSrc", "newTgt")]
            , newE       <$> [ ("h (src1)"  , "newSrc")
                             , ("newSrc", "m (tgt1)"  )
                             , ("e (src2)"  , "newTgt")
                             , ("newTgt", "g (tgt2)"  )
                             ]
            ]

-- Edge added: (b,e) -> (g, k)
-- Problem: The edge (b,e) is ancestral to (g,k) and so this is not allowed
hasE2NetworkNodeSrc :: Network
hasE2NetworkNodeSrc =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ existingN   <$>  ["root", "a", "c", "i", "j"
                               , "k", "n" , "o", "p", "q" ,"r"
                               ]
            , newN        <$> ["newSrc", "newTgt"]
            , contextualN <$> ["g (src1)", "h (src2)", "l (tgt1)", "m (tgt2)"]
            ]

    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a"), ("root", "b")
                  , ("a", "c"), ("a" ,"d"), ("b", "d"), ("b", "e")
                  , ("d", "f"), ("e", "g (src1)"), ("e", "h (src2)")
                  , ("f", "i"), ("f", "j")
                  , ("g (src1)", "k")
                  , ("h (src2)", "n")
                  , ("m (tgt2)", "o"), ("n", "o"), ("m (tgt2)", "p"), ("n", "q")
                  , ("o", "r")
                  ]
            , candidateE <$> [ ("newSrc", "newTgt")]
            , newE       <$> [ ("g (src1)"  , "newSrc")
                             , ("newSrc", "l (tgt1)"  )
                             , ("h (src2)"  , "newTgt")
                             , ("newTgt", "m (tgt2)"  )
                             ]
            ]

-- Edge added: (h,m) -> (n, o)
-- Problem: We cannot have a new edge into an existing network edge
-- as this leads to having a display tree with a new leaf
hasE2NetworkNodeTgt :: Network
hasE2NetworkNodeTgt =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ existingN   <$>  ["root", "a", "b", "c", "d", "e", "f", "g", "i", "j"
                               , "k", "l", "p", "q" ,"r"
                               ]
            , newN        <$> ["newSrc", "newTgt"]
            , contextualN <$> ["h (src1)", "n (src2)", "m (tgt1)", "o (tgt2)"]
            ]

    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a"), ("root", "b")
                  , ("a", "c"), ("a" ,"d"), ("b", "d"), ("b", "e")
                  , ("d", "f"), ("e", "g"), ("e", "h (src1)")
                  , ("f", "i"), ("f", "j")
                  , ("g", "k"), ("g", "l")
                  ,  ("h (src1)", "n (src2)")
                  , ("m (tgt1)", "o (tgt2)"), ("n (src2)", "o (tgt2)"), ("m (tgt1)", "p"), ("n (src2)", "q")
                  , ("o (tgt2)", "r")
                  ]
            , candidateE <$> [("newSrc", "newTgt")]
            , newE       <$> [ ("h (src1)"  , "newSrc")
                             , ("newSrc", "m (tgt1)"  )
                             , ("n (src2)"  , "newTgt")
                             , ("newTgt", "o (tgt2)"  )
                             ]
            ]



-- Edge added: (b,e) -> (g, k)
-- Problem: The edge (b,e) is ancestral to (g,k) and so this is not allowed
e1DescendantE2AncestralNetwork :: Network
e1DescendantE2AncestralNetwork =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ existingN   <$> ["root", "b", "d", "e", "g", "h", "j"
                              , "k", "l","m", "n" , "o", "p", "q", "r"
                              ]
            , newN        <$> ["newSrc", "newTgt"]
            , contextualN <$> ["a (src1)", "f (src2)", "c (tgt1)", "i (tgt2)"]
            ]

    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a (src1)"), ("root", "b")
                  , ("a (src1)" ,"d"), ("b", "d"), ("b", "e")
                  , ("d", "f (src2)"), ("e", "g"), ("e", "h")
                  , ("f (src2)", "j")
                  , ("g", "k"), ("g", "l")
                  , ("h", "m"), ("h", "n")
                  , ("m", "o"), ("n", "o"), ("m", "p"), ("n", "q")
                  , ("o", "r")
                  ]
            , candidateE <$> [("newSrc", "newTgt")]
            , newE       <$> [ ("a (src1)"  , "newSrc")
                             , ("newSrc", "c (tgt1)"  )
                             , ("f (src2)"  , "newTgt")
                             , ("newTgt", "i (tgt2)"  )
                             ]
            ]

-- Edge added: (b,e) -> (g, k)
-- Problem: The edge (b,e) is ancestral to (g,k) and so this is not allowed
e2DescendantE1AncestralNetwork :: Network
e2DescendantE1AncestralNetwork =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ existingN   <$>  ["root", "e", "g" , "h", "i", "j"
                               , "k", "l", "m", "n" , "o", "p", "q" ,"r"
                               ]
            , newN        <$> ["newSrc", "newTgt"]
            , contextualN <$> ["d (src1)", "a (src2)", "f (tgt1)", "c (tgt2)"]
            ]

    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a (src2)"), ("root", "b")
                  , ("a (src2)" ,"d (src1)"), ("b", "d (src1)"), ("b", "e")
                  , ("e", "g"), ("e", "h")
                  , ("f (tgt1)", "i"), ("f (tgt1)", "j")
                  , ("g", "k"), ("g", "l")
                  , ("h", "m"), ("h", "n")
                  , ("m", "o"), ("n", "o"), ("m", "p"), ("n", "q")
                  , ("o", "r")
                  ]
            , candidateE <$> [("newSrc", "newTgt")]
            , newE       <$> [ ("d (src1)"  , "newSrc")
                             , ("newSrc", "f (tgt1)"  )
                             , ("a (src2)"  , "newTgt")
                             , ("newTgt", "c (tgt2)"  )
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

-- Edge added: (b,e) -> (g, k)
-- Problem: The edge (b,e) is ancestral to (g,k) and so this is not allowed
e1NetworkEdgeComplementNodeAncestralToE2 :: Network
e1NetworkEdgeComplementNodeAncestralToE2 =
    (templateNodes, edges)
  where
    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a"), ("root", "b")
                  , ("a", "c"), ("a" ,"d"), ("b", "d"), ("b", "e")
                  , ("d", "f"), ("e", "g"), ("e", "h")
                  , ("f", "i"), ("f", "j")
                  , ("g", "k"), ("g", "l")
                  , ("h", "m"), ("h", "n")
                  , ("m", "o"), ("n", "o"), ("m", "p"), ("n", "q")
                  , ("o", "r")
                  ]
            , candidateE <$> [("newSrc", "newTgt")]
            , newE       <$> [ ("b (src1)"  , "newSrc")
                             , ("newSrc", "e (tgt1)"  )
                             , ("g (src2)"  , "newTgt")
                             , ("newTgt", "k (tgt2)"  )
                             ]
            ]



networks :: [(Network, FilePath)]
networks = [ (baseNetwork                   , "base-network"                   )
           , (hasE2NetworkNodeSrc           , "hasE2NetworkNodeSrc"           )
           , (hasE2NetworkNodeTgt           , "hasE2NetworkNodeTgt"           )
           , (e2AncestralEdgeNetwork        , "e2AncestralEdgeNetwork"        )
           , (e1HasE2SrcAncestralNodeNetwork, "e1HasE2SrcDescendantNodeNetwork")
           , (e1DescendantE2AncestralNetwork, "e1DescendantE2AncestralNetwork")
           , (e2DescendantE1AncestralNetwork, "e2DescendantE1AncestralNetwork")
           ]





--renderExampleNetwork :: String
--renderExampleNetwork = unlines
--                         [ "                        root"
--                         , "                      /     \
--                         , "                     a       b
--                         , "                   /  \     /  \
--                         , "                  c    \   /    e
--                         , "                         d     /  \
--                         , "                         |    g    \
--                         , "                         f   / \    \
--                                                    / \  k  l    \
--                         ,                         i   j          h
--                                                                 / \
--                                                                m   n
--                                                                 \ /
--                                                                  o
--                                                                  |
--                                                                  p
--
--
--                         ]




networkGraphParameters
 :: G.GraphvizParams
       String          -- vertex type
       NodeLabel       -- vertex label type
       EdgeLabel       -- edge label type
       ()              -- cluster type
       NodeLabel       -- cluster label type
networkGraphParameters = G.defaultParams {
    G.fmtNode  = \case
        (_, ExistingNodeLabel  ) -> colorAttribute  black
        (_, ContextualNodeLabel) -> colorAttribute  red
        (_, NewNodeLabel       ) -> colorAttribute  blue,

    G.fmtEdge = \case
        (_, _, ExistingEdgeLabel)  -> colorAttribute black
        (_, _, NewEdgeLabel     )  -> colorAttribute red
        (_, _, CandNetEdgeLabel )  -> colorAttribute blue
        }
  where
    colorAttribute color = [ G.Color $ G.toColorList [ color ] ]
    black = G.RGB 0 0 0
    red   = G.RGB 30 144 255
    blue  = G.RGB 204 2 2


template :: Network
template =
    (templateNodes, edges)
  where
    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a"), ("root", "b")
                  , ("a", "c"), ("a" ,"d"), ("b", "d"), ("b", "e")
                  , ("d", "f"), ("e", "g"), ("e", "h")
                  , ("f", "i"), ("f", "j")
                  , ("g", "k"), ("g", "l")
                  , ("h", "m"), ("h", "n")
                  , ("m", "o"), ("n", "o"), ("m", "p"), ("n", "q")
                  , ("o", "r")
                  ]
            , candidateE <$> [("newSrc", "newTgt")]
            , newE       <$> [ (" (src1)"  , "newSrc")
                             , ("newSrc", " (tgt1)"  )
                             , (" (src2)"  , "newTgt")
                             , ("newTgt", " (tgt2)"  )
                             ]
            ]


templateNodes :: [Node]
templateNodes = fold
        [ existingN   <$>  ["root", "a", "c", "d", "e", "f", "g" , "h", "i", "j"
                           , "k", "l", "m", "n" , "o", "p", "q", "r"
                           ]
        , newN        <$> ["newSrc", "newTgt"]
        , contextualN <$> ["b (src1)", "g (src2)", "e (tgt1)", "k (tgt2)"]
        ]


