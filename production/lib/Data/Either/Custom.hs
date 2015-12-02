module Data.Either.Custom where

import Control.Monad.Trans.Either
import Data.Either                (partitionEithers)
import Data.Semigroup

-- | Allows for the coalescence of Left values that form a Semigroup.
--   Overrides short-circuiting behaviour of the standard monadic `sequence` definition.
--   The validation context will evaluate the entire [Either e a] returning a Right value
--   if and only if only Right values were present in the list of Eithers. If any Left
--   values were present in the list of Eithers then a Left value is returned consisting
--   of the an order preservign fold using the semigroup operation `(<>)`.
eitherValidation :: Semigroup e => [Either e a] -> Either e [a]
eitherValidation xs =
  case partitionEithers xs of
    ([] , res) -> pure res
    (err, _  ) -> Left $ foldl1 (<>) err

-- |
eitherTValidation :: (Monad m, Semigroup e) => [EitherT e m a] -> EitherT e m [a]
eitherTValidation = EitherT . fmap eitherValidation . sequence . fmap runEitherT
