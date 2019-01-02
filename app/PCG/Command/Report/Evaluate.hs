{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module PCG.Command.Report.Evaluate
  ( evaluate
  ) where

import Bio.Graph
import Control.Monad.IO.Class
import Data.Compact                (Compact, getCompact)
import Data.List.NonEmpty
import PCG.Command.Report
import PCG.Command.Report.GraphViz
import Text.XML
import TextShow (TextShow (showtl), printT)
import Data.Render.Utility (writeFileT)
import System.IO (IOMode(AppendMode, WriteMode))
import qualified Data.Text.Lazy as Lazy
import Data.Foldable (traverse_)



evaluate :: ReportCommand -> GraphState -> SearchState
evaluate (ReportCommand format target) stateValue = do
    _ <- case generateOutput stateValue format of
           ErrorCase    errMsg  -> fail errMsg
           MultiStream  streams ->
             traverse_
               (\(filepath, content) -> liftIO $ (writeFileT WriteMode filepath content))
               streams
           SingleStream output  ->
             let op = case target of
                        OutputToStdout   -> printT
                        OutputToFile f w ->
                          case w of
                            Append    ->  writeFileT AppendMode f
                            Overwrite ->  writeFileT WriteMode  f
             in  liftIO (op output)
    pure stateValue


-- TODO: Redo reporting
--generateOutput :: t1 -> t -> FileStreamContext
{-
generateOutput :: DirectOptimizationPostOrderDecoration z a
               => Either t (PhylogeneticSolution (PhylogeneticDAG e n u v w x y z))
               -> OutputFormat
               -> FileStreamContext
-}
{-
generateOutput :: (Show c, Show t, ToXML c)
               => Either t c
               -> OutputFormat
               -> FileStreamContext
-}
generateOutput
  :: GraphState
  -> OutputFormat
  -> FileStreamContext
generateOutput g' format =
  case format of
    Data {}    -> SingleStream $ either showtl showtl g
    XML  {}    -> SingleStream $ either showtl (showtl . ppTopElement . toXML) g
    DotFile {} -> SingleStream $ generateDotFile g'
    _          -> ErrorCase "Unrecognized 'report' command"
  where
    g = getCompact g'

--generateOutput :: StandardSolution -> OutputFormat -> FileStreamContext
--generateOutput g (CrossReferences fileNames)   = SingleStream $ taxonReferenceOutput g fileNames
--generateOutput (Right g) DynamicTable               {} = SingleStream $ outputDynamicCharacterTablularData g
--generateOutput g Metadata                   {} = SingleStream $ metadataCsvOutput g
{-
generateOutput g ImpliedAlignmentCharacters {} =
  case getForests g of
    [] -> ErrorCase "The graph contains an empty forest."
    _  ->
      case dynamicCharacterCount g of
        0 -> ErrorCase "There are no dynamic characters in the graph. Cannot construct an implied alignment on a graph which contains no dynamic characters."
        _ ->
          case iaOutput . iaSolution $ addOptimization g of
            [] -> ErrorCase "There were no Dynamic homology characters on which to perform an implied alignment."
            zs -> MultiStream $ fromList zs
-}
{-
  case getForests g of
    [] -> ErrorCase "The graph contains an empty forest."
    _  ->
      case dynamicCharacterCount g of
        0 -> ErrorCase "There are no dynamic characters in the graph. Cannot construct an implied alignment on a graph which contains no dynamic characters."
        _ ->
          let g' = addOptimization g
          in case iaSolution' g' of
               [] -> ErrorCase "The result of the Implied Aligmnment returned an empty graph. (No dynamic homology characters?)"
               ys ->
                  case iaOutput' ys g' of
                    [] -> ErrorCase "There were no Dynamic homology characters on which to perform an implied alignment."
                    zs -> MultiStream $ fromList zs
-}

{--
showWithTotalEdgeCost
  :: ( HasSingleDisambiguation z c
     , EncodableDynamicCharacter c
     , Exportable c
     , Show m
     , Show e
     , Show n
     , Show u
     , Show v
     , Show w
     , Show x
     , Show y
     , Show z
     , HasCharacterCost   u Double
     , HasCharacterCost   v Word
     , HasCharacterCost   w Word
     , HasCharacterCost   x Word
     , HasCharacterCost   y Word
     , HasCharacterCost   z Word
     , Element c ~ DynamicCharacterElement
     )
  => PhylogeneticSolution (PhylogeneticDAG2 m e n u v w x y z)
  -> String
showWithTotalEdgeCost x = unlines
    [ show $ fmap totalEdgeCosts . toNonEmpty <$> phylogeneticForests x
    , show x
    ]
--}

type FileContent = Lazy.Text


data FileStreamContext
   = ErrorCase    String
   | SingleStream FileContent
   | MultiStream  (NonEmpty (FilePath, FileContent))
