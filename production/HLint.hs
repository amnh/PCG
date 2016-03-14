import "hint" HLint.Default
import "hint" HLint.Dollar
-- import "hint" HLint.Generalise

warn = x *> pure y ==> x $> y
warn = pure y <* x ==> x <$ y
warn = return x ==> pure x
