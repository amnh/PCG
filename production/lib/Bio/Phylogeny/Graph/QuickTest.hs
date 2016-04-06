import Bio.Phylogeny.Graph
import Bio.Phylogeny.Graph.Output
import Bio.Phylogeny.Graph.Random

main = do
    randTree <- arbitrary :: Tree
    hPutStrLn "bye now"
