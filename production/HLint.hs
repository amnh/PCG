import "hint" HLint.Default
import "hint" HLint.Dollar
import "hint" HLint.Builtin.All
-- import "hint" HLint.Generalise

import        Data.Monoid
import        Control.Monad


warn = x *> pure y ==> x $> y
warn = pure y <* x ==> x <$ y
warn = return x ==> pure x

-- warn = concatMap ==> (=<<)

warn = liftM ==> fmap
    where _ = noQuickCheck

warn = map ==> fmap

