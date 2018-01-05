
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module PCG.Command.Report.Evaluate
  ( evaluate
  ) where


import           Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
--import           Analysis.ImpliedAlignment.Standard
--import           Analysis.ImpliedAlignment
--import           Analysis.Parsimony.Binary.Optimization
import           Bio.Character.Decoration.Dynamic
import           Bio.Character.Encodable
--import           Bio.Metadata
import           Bio.Graph
import           Bio.Graph.PhylogeneticDAG
import           Control.DeepSeq
import           Control.Monad.IO.Class
--import           Control.Monad.Logger
--import           Data.Foldable
import           Data.List.NonEmpty
import           Data.MonoTraversable
import           Data.Semigroup.Foldable
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


evaluate :: Command -> SearchState -> SearchState
evaluate (REPORT (ReportCommand format target)) old = do
    stateValue <- force old
    case generateOutput stateValue format of
     ErrorCase    errMsg  -> fail errMsg
     MultiStream  streams -> old <* sequenceA (liftIO . uncurry writeFile <$> streams)
     SingleStream output  ->
       let op = case target of
                  OutputToStdout   -> putStr
                  OutputToFile f w ->
                    case w of
                      Append    -> appendFile f
                      Overwrite ->  writeFile f
       in  liftIO (op output) *> old

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
     , HasSymbolChangeMatrix   z (Word -> Word -> Word)
     , EncodableDynamicCharacter c
     , Ord (Element c), Show e, Show n, Show u
     , Show v, Show w, Show x, Show y, Show z
     , HasCharacterCost   u Double
     , HasCharacterCost   v Word
     , HasCharacterCost   w Word
     , HasCharacterCost   x Word
     , HasCharacterCost   y Word
     , HasCharacterCost   z Word
     , HasCharacterWeight u Double
     , HasCharacterWeight v Double
     , HasCharacterWeight w Double
     , HasCharacterWeight x Double
     , HasCharacterWeight y Double
     , HasCharacterWeight z Double
     ) 
  => PhylogeneticSolution (PhylogeneticDAG2 e n u v w x y z) 
  -> String
showWithTotalEdgeCost x = unlines
    [ show $ fmap (totalEdgeCosts naiveDO) . toNonEmpty <$> phylogeneticForests x
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
