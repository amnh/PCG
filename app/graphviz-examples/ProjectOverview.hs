{-# LANGUAGE LambdaCase #-}

module ProjectOverview where

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


type Node = (String, ())

type Edge = (String, String, ())

node :: String -> Node
node s = (s, ())

edge :: (String, String) -> Edge
edge (s,t) = (s,t, ())

type Network = ([Node], [Edge])

-- |
-- Parameters for how to render a display tree for a network.
networkGraphParameters
 :: G.GraphvizParams
       String          -- vertex type
       ()              -- vertex label type
       ()     -- edge label type
       ()              -- cluster type
       ()              -- cluster label type
networkGraphParameters = G.defaultParams {
    G.fmtNode = const $  colorAttribute  green <> box
    ,
    G.fmtEdge = const $ colorAttribute blue
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
    box   = [G.Shape G.BoxShape] <> [G.Area 0.5]



--------------------------------------
--            Examples              --
--------------------------------------

-- |
-- A high-level diagram of our project
overview :: [(Network, FilePath)]
overview = [ (projectOverview  , "project-overview")
           ]

-- |
-- A high-level diagram of our project
projectOverview :: Network
projectOverview =
    (nodes, edges)
  where
    nodes :: [Node]
    nodes = node <$>
             [scriptingLanguage, parsing, unification, treeSearch, serialisation]
            

    edges :: [Edge]
    edges = edge <$>
            [ (scriptingLanguage, parsing)
            , (parsing, unification)
            , (unification, treeSearch)
            , (treeSearch, treeSearch)
            , (treeSearch, serialisation)
            ]

    scriptingLanguage, parsing, unification, treeSearch, serialisation :: String
    scriptingLanguage = unlines
      [ "Scripting language"
      , "  Based on optparse-applicative"
      ]

    parsing = unlines
      [ "Parsing"
      , "  Based on megaparsec"
      ]

    unification = unlines
      [ "Unification"
      , "  Process and validate user input"
      ]

    treeSearch = unlines
      [ "Tree Search"
      , "  Core data structures: characters, networks etc."
      , "  Scoring and tree search occurs here"
      ]

    serialisation = unlines
      [ "Serialisation"
      , " - Output formats: textual, dot, newick, csv, binary etc."
      ]
