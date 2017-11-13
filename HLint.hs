import "hint" HLint.Default
import "hint" HLint.Dollar
import "hint" HLint.Builtin.All
-- import "hint" HLint.Generalise

import        Control.Monad
import        Control.Parallel.Custom
import        Control.Parallel.Strategies
import        Data.Maybe
import        Data.Semigroup


warn "Use constant Functor context instead of more constrained Applicative context" = x *> pure y ==> x $> y
warn "Use constant Functor context instead of more constrained Applicative context" = pure y <* x ==> x <$ y
warn "The function 'return' is superflous, always use the function 'pure'" = return x ==> pure x

-- warn = concatMap ==> (=<<)

warn "Use less constrained function" = liftM ==> fmap
  where _ = noQuickCheck

warn "Use less constrained function" = map ==> fmap

warn "Use Foldable instance definition to generate list" = maybeToList ==> toList

warn "Use more general custom function" = parMap ==> parmap

warn "Use the Semigroup operator instead of less general concat operator" = (++) ==> (<>)

warn "Use the Semigroup operator instead of less general concat operator" = lhs ++ rhs ==> lhs <> rhs
