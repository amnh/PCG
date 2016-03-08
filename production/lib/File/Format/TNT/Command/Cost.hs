{-# LANGUAGE FlexibleContexts #-}
module File.Format.TNT.Command.Cost where

import           Data.Functor             (($>))
import           Data.Foldable            (toList,maximumBy)
import           Data.DList               (DList,append)
import qualified Data.DList         as DL (toList,fromList)
import           Data.IntMap              (IntMap,insertWith)
import qualified Data.IntMap        as IM (lookup)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (fromList)
import           Data.Matrix.NotStupid    (Matrix,matrix)
import           Data.Maybe               (fromJust,fromMaybe)
import           Data.Ord                 (comparing)
import           Data.Vector              ((!))
import           File.Format.TNT.Internal
import           Text.Megaparsec
import           Text.Megaparsec.Custom   (double)
import           Text.Megaparsec.Prim     (MonadParsec)

data TransitionCost
   = TransitionCost
   { origins   :: NonEmpty Char
   , symetric  :: Bool
   , terminals :: NonEmpty Char
   , costValue :: Double
   } deriving (Eq,Show)

-- | Parses a Cost command that consists of:
--
--  * A single specification of the character state change
--
--  * One or more character indicies or index ranges of affected characters
costCommand :: MonadParsec s m Char => m Cost
costCommand = costHeader *> costBody <* symbol (char ';')

-- | Consumes the superflous heading for a CCODE command.
costHeader :: MonadParsec s m Char => m ()
costHeader = symbol $ keyword "cost" 2

costBody :: MonadParsec s m Char => m Cost
costBody = do
      idx <- symbol characterIndicies
      _   <- symbol $ char '='
      transitions <- NE.fromList <$> some costDefinition
      pure . Cost idx $ condenseToMatrix transitions

condenseToMatrix :: (Foldable f, Functor f) => f TransitionCost -> Matrix Double
condenseToMatrix costs = matrix dimensions dimensions value
  where
    dimensions   = succ . fromJust $ maximumState `indexOf` discreteStateValues
    maximumState = maximum $ f <$> costs
      where
        f tc = max (maximum $ origins tc) (maximum $ terminals tc)
    value (i,j) = fromMaybe 1 $ foldl f Nothing costs
      where
        i' = discreteStateValues ! i
        j' = discreteStateValues ! j
        f m x
          | inject || (symetric x && surject) = Just $ costValue x
          | otherwise                         = m
          where
            inject  = i' `elem` (origins x) && j' `elem` (terminals x)
            surject = j' `elem` (origins x) && i' `elem` (terminals x)
    indexOf :: (Eq a, Foldable f) => a -> f a -> Maybe Int
    indexOf e = snd . foldl f (0, Nothing)
      where
        f a@(_, Just _ ) _ = a
        f a@(i, Nothing) x
          | x == e    = (i  , Just i )
          | otherwise = (i+1, Nothing)

costDefinition :: MonadParsec s m Char => m TransitionCost
costDefinition = TransitionCost
             <$> symbol costStates
             <*> symbol costRelation
             <*> symbol costStates
             <*> symbol double
  where
    costRelation :: MonadParsec s m Char => m Bool
    costRelation = (char '>' $> False) <|> (char '/' $> True )
    costStates :: MonadParsec s m Char => m (NonEmpty Char)
    costStates = NE.fromList <$> (singleState <|> manyStates)
      where
        singleState = pure <$> characterStateChar
        manyStates  = between open close (some characterStateChar)
        open  = symbol $ char '['
        close = symbol $ char ']'

    
