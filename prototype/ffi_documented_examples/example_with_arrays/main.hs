-- from http://stackoverflow.com/questions/35050825/haskell-ffi-passing-array-of-c-structs-in-and-out


module Main (main) where

import RGB

main =
 do
    let a = [RGB {r = 1.0, g = 1.0, b = 1.0},
             RGB {r = 2.0, g = 2.0, b = 2.0},
             RGB {r = 3.0, g = 3.0, b = 3.0}]
    let l = length a
    print a
    -- b <- rgbTest a
    -- print b

    c <- rgbAlloc a
    rgbTest2 c l
    rgbTest2 c l
    d <- rgbPeek c l
    print d
    return ()