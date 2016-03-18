-- Use the standard hint files
import "hint" HLint.Default
import "hint" HLint.Builtin.All

import Data.Monoid
import Control.Monad

-- warn = concatMap ==> (=<<)

warn = liftM ==> fmap
    where _ = noQuickCheck

warn = map ==> fmap
          
