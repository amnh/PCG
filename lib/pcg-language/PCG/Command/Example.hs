module PCG.Command.Example where


import PCG.Syntax.Combinators
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE


data RRead = Read (NonEmpty FileSpec) deriving (Show)

data FileSpec
    = Unspecified String
    | Nucleotides String
    | AminoAcids  String
    | TCMFile     String String
    | FunnyValues Int String Double Time
    deriving (Show)
