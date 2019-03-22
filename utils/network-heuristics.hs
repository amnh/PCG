{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
module Main where

import Bio.Graph.ReferenceDAG.Utility
import Bio.Graph.ReferenceDAG.Internal
import           Control.Monad.Loops

main :: IO ()
main = print "hoho"


type TopGraph = RefernceDAG () () ()


samples :: Int
samples = 100

runExamples :: MonadRandom m => m Int ->  m [Int]
runExamples example = replciateM samples example


numberOfEdges :: forall m . MonadRandom m =>  TopGraph -> m Int
numberOfEdges dag = do
  finalDAG <- iterateUntilM stoppingCondition addNetworkEdge (binTree, False)
  pure refDAG
    where
      stoppingCondition :: TopGraph -> Bool
      stoppingCondition = null . candidateNetworkEdges


      addNetworkEdge :: TopGraph -> m TopGraph
      addNetworkEdge dag =
        do
          let networkEdges = toList . candidateNetworkEdges $ dag
          edge <- randomListElement 
            
            newDAG = connectEdge dag combine (<>) sourceEdge targetEdge
              pure (newRefDAG, False)
        where
          combine n1 n2 n3 = n1 <> n2 <> n3
    

randomListElement :: MonadRandom m => [a] -> a
randomListElement =
  \case
    [] -> error "Tried to access random element of empty list"
    ls ->
      do
        ind <- getRandomR (0, length ls - 1)
        pure $ ls !! ind
    

data SummaryStat = SummaryStat
  { meanS              :: Double
  , standardDeviationS :: Double
  , minS               :: Maybe Int
  , maxS               :: Maybe Int
  }

instance Show SummaryStat where
  show SummaryStat{..} =
    unlines
      [ "mean: " <> show meanS
      , "standard deviation: " <> show standardDeviationS
      , "minimum: " <> show (fromJust minS)
      , "maximum: " <> show (fromJust maxS)
      ]

summarise :: [Int] -> SummaryStat
summarise xs = SummaryStat{..}
  where
    (meanS, standardDeviationS, minS, maxS)
      = L.fold summaryFold xs

    summaryFold = (,,,) <$> mean' <*> std' <*> L.minimum <*> L.maximum

mean' :: Fractional a =>  L.Fold Int a
mean' = L.premap fromIntegral L.mean

std' :: Floating a =>  L.Fold Int a
std' = L.premap fromIntegral L.std
