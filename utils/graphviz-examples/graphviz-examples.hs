{-# LANGUAGE LambdaCase      #-}

module Main where

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G
import System.FilePath.Posix ((<.>))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import Data.Foldable (traverse_, Foldable(fold))


main :: IO ()
main = do
    putStrLn "Graph"
    createDirectoryIfMissing False "graphviz-examples"
    setCurrentDirectory "graphviz-examples"
    traverse_ makeDotFile networks

    
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


baseNetwork :: Network
baseNetwork = 
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = existingN <$> ["root", "a", "b" , "c", "d", "e", "f", "g", "h", "i", "j"]
    
    edges :: [Edge]
    edges = existingE <$>
              [ ("root", "a"), ("root", "b")
              , ("a", "c"), ("a" ,"d"), ("b", "d"), ("b", "e")
              , ("d", "f"), ("e", "g"), ("e", "h")
              ,  ("g", "i"), ("g", "j")
              ]
               


ancestralNetwork :: Network
ancestralNetwork = 
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ existingN   <$>  ["root", "a", "c", "d", "f", "h", "j"]
            , newN        <$> ["newSrc", "newTgt"]
            , contextualN <$> ["src1", "src2", "tgt1", "tgt2"]
            ]
    
    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a"), ("root", "src1")
                  , ("a", "c"), ("a" ,"d"), ("src1", "d")
                  , ("d", "f"), ("tgt1", "src2"), ("tgt1", "h")
                  , ("src2", "j")
                  ]
            , candidateE <$> [("newSrc", "newTgt")]
            , newE       <$> [ ("src1"  , "newSrc")
                             , ("newSrc", "tgt1"  )
                             , ("src2"  , "newTgt")
                             , ("newTgt", "tgt2"  )
                             ]
            ]

networkAncestralNetwork :: Network
networkAncestralNetwork = 
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = fold
            [ existingN   <$>  ["root", "a", "c", "d", "f", "h", "j"]
            , newN        <$> ["newSrc", "newTgt"]
            , contextualN <$> ["src1", "src2", "tgt1", "tgt2"]
            ]
    
    edges :: [Edge]
    edges = fold
            [ existingE  <$>
                  [ ("root", "a"), ("root", "src1")
                  , ("a", "c"), ("a" ,"d"), ("src1", "d")
                  , ("d", "f"), ("tgt1", "src2"), ("tgt1", "h")
                  , ("src2", "j")
                  ]
            , candidateE <$> [("newSrc", "newTgt")]
            , newE       <$> [ ("src1"  , "newSrc")
                             , ("newSrc", "tgt1"  )
                             , ("src2"  , "newTgt")
                             , ("newTgt", "tgt2"  )
                             ]
            ]
            

    
    
networks :: [(Network, FilePath)]
networks = [(baseNetwork, "baseNetwork"), (ancestralNetwork, "ancestralNetwork")]




--renderExampleNetwork :: String
--renderExampleNetwork = unlines
--                         [ "                        root"
--                         , "                      /     \
--                         , "                     a       b
--                         , "                   /  \     /  \
--                         , "                  c    \   /    e
--                         , "                         d     /  \
--                         , "                         |    g   h
--                         , "                         f   / \
--                                                         i  j 
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
        (_, ContextualNodeLabel) -> colorAttribute  blue
        (_, NewNodeLabel       ) -> colorAttribute  red,
  
    G.fmtEdge = \case
        (_, _, ExistingEdgeLabel)  -> colorAttribute black
        (_, _, NewEdgeLabel     )  -> colorAttribute blue
        (_, _, CandNetEdgeLabel )  -> colorAttribute red
        }  
  where
    colorAttribute color = [ G.Color $ G.toColorList [ color ] ]
    black = G.RGB 0 0 0
    red   = G.RGB 30 144 255
    blue  = G.RGB 204 2 2
