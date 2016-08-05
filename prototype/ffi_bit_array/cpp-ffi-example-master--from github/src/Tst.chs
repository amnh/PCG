module Tst (makeA, runA) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

#include "a.h"

{#pointer APtr as A foreign finalizer tstFreeA#}
{#fun tstMakeA as makeA {`Int'} -> `A'#}
{#fun tstRunA as runA {`A', `Int'} -> `()'#}
