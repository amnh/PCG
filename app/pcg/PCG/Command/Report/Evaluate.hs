{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module PCG.Command.Report.Evaluate
  ( evaluate
  ) where


import Bio.Graph
import Control.Evaluation
import Control.Monad.IO.Class
import Control.Monad.Trans.Validation
import Data.FileSource                (FileSource)
import Data.FileSource.IO
import Data.Foldable                  (traverse_)
import Data.Functor                   (($>))
import Data.List.NonEmpty             (NonEmpty (..))
import Data.String                    (IsString (fromString))
import Data.Validation
import PCG.Command.Report
import PCG.Command.Report.Distance
import PCG.Command.Report.GraphViz
import PCG.Command.Report.Metadata
import Prelude                        hiding (appendFile, writeFile)
import Text.XML
import TextShow                       (TextShow (showtl))


data FileStreamContext
   = ErrorCase    String
   | SingleStream FileStream
   | MultiStream  (NonEmpty (FileSource, FileStream))


evaluate :: ReportCommand -> GraphState -> SearchState
evaluate (ReportCommand format target) stateValue = reportStreams $> stateValue
  where
    reportStreams =
      case generateOutput stateValue format of
           ErrorCase    errMsg  -> failWithPhase Outputting errMsg
           MultiStream  streams -> renderMultiStream streams
           SingleStream output  -> renderSingleStream target output


renderMultiStream :: NonEmpty (FileSource, FileStream) -> EvaluationT GlobalSettings IO ()
renderMultiStream = runOutputStream . traverse_ (uncurry writeFile)


renderSingleStream :: OutputTarget -> FileStream -> EvaluationT GlobalSettings IO ()
renderSingleStream target output = runOutputStream $
    case target of
      OutputToStdout   -> writeSTDOUT output
      OutputToFile f w ->
        case w of
          Append    -> appendFile f output
          Overwrite ->  writeFile f output
          Move      ->  writeFileWithMove f output


runOutputStream :: ValidationT OutputStreamError IO () -> EvaluationT GlobalSettings IO ()
runOutputStream outputValidation = do
    result <- liftIO $ runValidationT outputValidation
    case result of
      Failure errMsg -> failWithPhase Outputting errMsg
      Success _      -> pure ()


generateOutput
  :: GraphState
  -> OutputFormat
  -> FileStreamContext
generateOutput g format =
    case format of
      Data     {} -> SingleStream . streamText $ either showtl showtl g
      XML      {} -> SingleStream . streamText $ either showtl (fromString . ppTopElement . toXML) g
      DotFile  {} -> SingleStream . streamText $ generateDotFile g
      Metadata {} -> either
                       (const $ ErrorCase "No metadata in topological solution")
                       (SingleStream . streamText . outputMetadata)
                       g
      _           -> ErrorCase "Unrecognized 'report' command"
    DistanceMatrix {} -> either
                           (const $ ErrorCase "No distance matrix in topological solution")
                           (SingleStream . streamText . outputDistanceMatrix)
                           g


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
