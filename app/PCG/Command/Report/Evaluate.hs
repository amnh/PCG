
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module PCG.Command.Report.Evaluate
  ( evaluate
  ) where


import           Analysis.Parsimony.Dynamic.DirectOptimization
--import           Analysis.ImpliedAlignment.Standard
--import           Analysis.ImpliedAlignment
--import           Analysis.Parsimony.Binary.Optimization
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Encodable
import           Bio.Character.Exportable
import           Bio.Metadata.CharacterName
import           Bio.Graph
import           Bio.Graph.PhylogeneticDAG
import           Control.Monad.IO.Class
--import           Control.Monad.Logger
--import           Data.Foldable
import           Data.List.NonEmpty
import           Data.MonoTraversable
import           Data.Semigroup.Foldable
import           Data.TCM.Memoized
import           PCG.Command.Report
--import           PCG.Command.Report.DynamicCharacterTable
import           PCG.Command.Report.GraphViz
--import           PCG.Command.Report.ImpliedAlignmentFasta
--import           PCG.Command.Report.Internal
--import           PCG.Command.Report.Metadata
--import           PCG.Command.Report.Newick
--import           PCG.Command.Report.TaxonMatrix
import           PCG.Syntax (Command(..))
import           Text.XML
-- import           Text.XML.Light

--import Bio.Graph.ReferenceDAG
--import Data.Semigroup
--import Debug.Trace


evaluate :: Command -> GraphState -> SearchState
evaluate (REPORT (ReportCommand format target)) stateValue = do
    _ <- case generateOutput stateValue format of
           ErrorCase    errMsg  -> fail errMsg
           MultiStream  streams -> sequence_ (liftIO . uncurry writeFile <$> streams)
           SingleStream output  ->
             let op = case target of
                        OutputToStdout   -> putStr
                        OutputToFile f w ->
                          case w of
                            Append    -> appendFile f
                            Overwrite ->  writeFile f
             in  liftIO (op output)
    pure stateValue

evaluate _ _ = fail "Invalid READ command binding"


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
--generateOutput :: StandardSolution -> OutputFormat -> FileStreamContext
--generateOutput g (CrossReferences fileNames)   = SingleStream $ taxonReferenceOutput g fileNames
generateOutput g Data                       {} = SingleStream $ either show showWithTotalEdgeCost g
generateOutput g XML                        {} = SingleStream $ either show (ppTopElement . toXML) g
generateOutput g DotFile                    {} = SingleStream $ generateDotFile g
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

generateOutput _ _ = ErrorCase "Unrecognized 'report' command"


showWithTotalEdgeCost 
  :: ( HasSingleDisambiguation z c
     , HasDenseTransitionCostMatrix  z (Maybe DenseTransitionCostMatrix)
     , HasSparseTransitionCostMatrix z MemoizedCostMatrix
     , EncodableDynamicCharacter c
     , Exportable c
     , Exportable (Element c)
     , Ord (Element c)
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
     , HasCharacterName   u CharacterName
     , HasCharacterName   v CharacterName
     , HasCharacterName   w CharacterName
     , HasCharacterName   x CharacterName
     , HasCharacterName   y CharacterName
     , HasCharacterName   z CharacterName
     , HasCharacterWeight u Double
     , HasCharacterWeight v Double
     , HasCharacterWeight w Double
     , HasCharacterWeight x Double
     , HasCharacterWeight y Double
     , HasCharacterWeight z Double
     , HasTraversalFoci   z (Maybe TraversalFoci)
     ) 
  => PhylogeneticSolution (PhylogeneticDAG2 e n u v w x y z) 
  -> String
{-
showWithTotalEdgeCost x | trace ("Before Report Rendering: " <>
                                   (unlines . fmap
                                      (unlines . fmap (\(PDAG2 dag) -> referenceRendering dag) . toList
                                      ) $ toList (toNonEmpty <$> phylogeneticForests x)
                                   )
                                ) False = undefined
-}
showWithTotalEdgeCost x = unlines
    [ show $ fmap totalEdgeCosts . toNonEmpty <$> phylogeneticForests x
    , show x
    ]


type FileContent = String


data FileStreamContext
   = ErrorCase    String
   | SingleStream FileContent
   | MultiStream  (NonEmpty (FilePath,FileContent))

{-
dynamicCharacterCount :: MetadataSolution m StandardMetadata => m -> Int
dynamicCharacterCount = foldl' f 0 . getMetadata
  where
    f n e = if   getType e == DirectOptimization
            then n + 1
            else n
-}
