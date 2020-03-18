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

-- Expose file parsers and related data types
    , "^File.Format.Dot.*$"
    , "^File.Format.Fasta.*$"
    , "^File.Format.Fastc.*$"
    , "^File.Format.Newick.*$"
    , "^File.Format.Nexus.*$"
    , "^File.Format.TNT.*$"
    , "^File.Format.TransitionCostMatrix.*$"

-- Ignore the incomplete Graph sub-library
    , "^Data.Graph.*$"
    ]

, type-class-roots = True
}