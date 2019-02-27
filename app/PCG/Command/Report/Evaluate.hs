{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module PCG.Command.Report.Evaluate
  ( evaluate
  ) where


import           Bio.Graph
import           Control.Monad               (when)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy        as BS
import           Data.Char
import           Data.Compact                (getCompact)
import           Data.Foldable               (traverse_)
import           Data.Functor                (($>))
import           Data.List                   (isPrefixOf)
import           Data.List.NonEmpty          (NonEmpty (..))
import           Data.Render.Utility         (writeFileT)
import           Data.String                 (IsString (fromString))
import qualified Data.Text.Lazy              as Lazy
import           PCG.Command.Report
import           PCG.Command.Report.GraphViz
import           PCG.Command.Report.Metadata
import           System.Directory
import           System.FilePath.Posix
import           System.IO                   (IOMode (AppendMode, WriteMode))
import           Text.XML
import           TextShow                    (TextShow (showtl), printT)


data  FileContent
    = T Lazy.Text
    | B BS.ByteString


data FileStreamContext
   = ErrorCase    String
   | SingleStream FileContent
   | MultiStream  (NonEmpty (FilePath, FileContent))


printFileContent :: FileContent -> IO ()
printFileContent (T s) = printT s
printFileContent (B s) = BS.putStr s


writeFileContent :: FilePath -> FileContent -> IO ()
writeFileContent f (T s) = writeFileT WriteMode f s
writeFileContent f (B s) = BS.writeFile f s

appendFileContent :: FilePath -> FileContent -> IO ()
appendFileContent f (T s) = writeFileT AppendMode f s
appendFileContent f (B s) = BS.appendFile f s


evaluate :: ReportCommand -> GraphState -> SearchState
evaluate (ReportCommand format target) stateValue = reportStreams $> stateValue
  where
    reportStreams =
      case generateOutput stateValue format of
           ErrorCase    errMsg  -> fail errMsg
           MultiStream  streams -> traverse_ (liftIO . uncurry writeFileContent) streams
           SingleStream output  ->
             liftIO $ case target of
                        OutputToStdout   -> printFileContent output
                        OutputToFile f w ->
                          case w of
                            Append    -> appendFileContent f output
                            Overwrite ->  writeFileContent f output
                            Move      -> safelyMoveFile f *> writeFileContent f output


-- |
-- Checks to see if the supplied 'FilePath' exists.
--
-- If it does, it moves the existing file path, so that the supplied file path
-- can be written to without overwriting data.
--
-- The exisiting file path is renamed, adding a numeric suffix to the end. The
-- function will try to rename the existing file path by adding the suffix ".0",
-- however if that filepath also exists, it will add ".1", ".2", ".3", ",.4", etc.
-- The suffix added will be one greater than the highest existing numeric suffix.
safelyMoveFile :: FilePath -> IO ()
safelyMoveFile fp = do
    exists <- doesFileExist fp
    when exists $ do
        allFiles <- getCurrentDirectory >>= getDirectoryContents
        let prefixed = getFilePathPrefixes     allFiles
        let numbers  = getNumericSuffixes      prefixed
        let lastNum  = getLargestNumericSuffix numbers
        let nextNum  = lastNum + 1 :: Word
        let newName  = fp <> "." <> show nextNum
        renameFile fp newName
  where
    getFilePathPrefixes = fmap (drop (length fp)) . filter (fp `isPrefixOf`)

    getNumericSuffixes  = fmap tail . filter hasDotThenNumberSuffix . fmap takeExtension
      where
        hasDotThenNumberSuffix ('.':x:xs) = all isNumber $ x:xs
        hasDotThenNumberSuffix _          = False

    getLargestNumericSuffix    []  = -1
    getLargestNumericSuffix (x:xs) = maximum . fmap read $ x:|xs


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
    Data     {} -> SingleStream . T $ either showtl showtl g
    XML      {} -> SingleStream . T $ either showtl (fromString . ppTopElement . toXML) g
    DotFile  {} -> SingleStream . T $ generateDotFile g'
    Metadata {} -> either
                     (const $ ErrorCase "No metadata in topological solution")
                     (SingleStream . B . outputMetadata)
                     g
    _           -> ErrorCase "Unrecognized 'report' command"
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
