{ roots =
    [ "^Main.main$"

-- Ignore anything from the cabal auto-generated paths module
    , "^Paths_phylogenetic_component_graph.*$"

-- PCG language combinators hould all be exposed
    , "^PCG.Syntax.Combinators.*$"

-- Template Haskell false positives 
    , "^PCG.Software.Credits.authorsList$"
    , "^PCG.Software.Credits.fundingList$"

-- Expose CharacterName module
    , "^Data.CharacterName.*$"

-- Expose Evaluation module
    , "^Control.Evaluation.*$"

-- Expose Utility sub-library modules
    , "^Control.Parallel.Custom.*$"
    , "^Data.BitMatrix.*$"
    , "^Data.Foldable.Custom.*$"
    , "^Data.List.Utility.*$"
    , "^Data.Matrix.NotStupid.*$"
    , "^Data.MutualExclusionSet.*$"
    , "^Data.Pair.Strict.*$"
    , "^Data.Range.*$"
    , "^Data.UnionSet.*$"
    , "^Data.Vector.Memo.*$"
    , "^Data.Vector.NonEmpty.*$"
    , "^Data.MutualExclusionSet.*$"
    , "^Numeric.Extended.Real.*$"

-- Expose Alphabet sub-library module
    , "^Data.Alphabet.Internal.fromSymbolsWithStateNamesAndTCM$"
    , "^Data.Alphabet.Internal.fromSymbolsWithTCM$"
    , "^Data.Alphabet.Internal.getPermutionContext$"
    , "^Data.Alphabet.Special.isAlphabetDiscrete$"

-- Expose file parsers and related data types
    , "^File.Format.Dot.*$"
    , "^File.Format.Fasta.*$"
    , "^File.Format.Fastc.*$"
    , "^File.Format.Newick.*$"
    , "^File.Format.Nexus.*$"
    , "^File.Format.TNT.*$"
    , "^File.Format.TransitionCostMatrix.*$"

-- Expose analysis top level bindings
    , "^Analysis.Scoring.performDecoration$"
    , "^Analysis.Scoring.performFinalizationDecoration$"
    , "^Analysis.Scoring.performPostorderDecoration$"
    , "^Analysis.Scoring.performPreorderDecoration$"
    , "^Analysis.Scoring.scoreSolution$"

-- Expose pairwise string alignment utility function
    , "^Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal.handleMissingCharacterThreeway"

-- Expose debugging utility function on streams
    , "Bio.Character.Encodable.Stream.showStream"

-- Ignore the incomplete Graph sub-library
    , "^Data.Graph.*$"

-- Ignore Graphviz-Examples
    , "^DisplayTree.*$"
    , "^NetworkEdges.*$"
    , "^ProjectOverview.*$"

-- Expose Test Suites
    , "^Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Test.*$"
    , "^Analysis.Clustering.Test.*$"
    , "^Bio.Character.Encodable.Dynamic.Test.*$"
    , "^Bio.Character.Encodable.Static.Test.*$"
    , "^Bio.Graph.ReferenceDAG.Test.*$"
    , "^Bio.Graph.ReferenceDAG.Test.NetworkPropertyTests.*$"
    , "^Bio.Graph.ReferenceDAG.Test.NetworkUnitTests.*$"
    , "^Control.Evaluation.Test.*$"
    , "^Control.Monad.Trans.Validation.Test.*$"
    , "^Control.Parallel.Test.*$"
    , "^Data.Alphabet.Test.*$"
    , "^Data.BitMatrix.Test.*$"
    , "^Data.Graph.Test.*$"
    , "^Data.List.Test.*$"
    , "^Data.TCM.Test.*$"
    , "^Data.MutualExclusionSet.Test.*$"
    , "^File.Format.Fasta.Test.*$"
    , "^File.Format.Fastc.Test.*$"
    , "^File.Format.Newick.Test.*$"
    , "^File.Format.TNT.Test.*$"
    , "^File.Format.TransitionCostMatrix.Test.*$"
    , "^File.Format.VertexEdgeRoot.Test.*$"
    , "^Numeric.Cost.Test.*$"
    , "^Numeric.Extended.Natural.Test.*$"
    , "^Numeric.Extended.Real.Test.*$"
    , "^Numeric.NonNegativeAverage.Test.*$"
    , "^System.ErrorPhase.Test.*$"
    , "^Text.Megaparsec.Custom.Test.*$"
    
--
    , "^TestSuite.GeneratedTests.*$"
    , "^TestSuite.GoldenTests.*$"
    , "^TestSuite.ScriptTests.*$"
    , "^TestSuite.SubProcess.*$"

    , "^Benchmark.FASTA.Space.*$"
    , "^Benchmark.FASTA.Time.*$"
    , "^Benchmark.FASTC.Space.*$"
    , "^Benchmark.FASTC.Time.*$"
    , "^Benchmark.Newick.Space.*$"
    , "^Benchmark.Newick.Time.*$"
    , "^Benchmark.TCM.Space.*$"
    , "^Benchmark.TCM.Time.*$"
    , "^Benchmark.VER.Space.*$"
    , "^Benchmark.VER.Time.*$"
    
    , "^Data.Graph.Bench.*$"

    , "^Benchmark.StringAlignment.*$"
    , "^Data.MutualExclusionSet.Bench.*$"
    , "^Numeric.Extended.Natural.Bench.*$"
    ]

, type-class-roots = True
}