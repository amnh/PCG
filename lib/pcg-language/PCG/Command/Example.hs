module PCG.Command.Example where


import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.List.NonEmpty     as NE
import           PCG.Syntax.Combinators


newtype RRead = Read (NonEmpty FileSpec) deriving (Show)

data FileSpec
    = Unspecified String
    | Nucleotides String
    | AminoAcids  String
    | TCMFile     String String
    | FunnyValues Int String Double Time
    deriving (Show)
