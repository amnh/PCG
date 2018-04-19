#Changelog

**Note:** In all following, changes were made in both `ukkCommon.c` and `ukkCommon_for_python.c`, the latter of which is needed to do debugging using Python scripts. `ukk_common.h` and `ukk.checkp.c` are shared across both.

## 17/10/25

* Cloned
* Removed several unneeded compilation units.
* Removed `inline` directive
* Used modern `limits` library vs. `values`
* Edited `makefile`
    * Added new make command: `test_powell`
    * Removed extra cruft
    * Changed warnings
    * Updated to c11
* Got it to compile
* Tested with several dynamic characters that had failed my code. It ran.

## 17/10/27
* More code cleanup for legibility
* Added Python wrapper code for testing
    * Originally, `Astr`, `Bstr` & `Cstr` were globals, but I was unable to accept input strings due to allocation problems. So it forced me to make `Astr`, `Bstr` & `Cstr` local variables, with allocation happening in `powell_3D_align`. I put the three input characters into a struct, which I then passed around.
    * Removed original limit on input characters’ lengths.

## 17/10/27–31
* Valgrinded C code. Fixed all segfaults and bad memory reads. They were cause by my not sending the "global" struct through to `traceback`. Not sure why the compiler wasn’t bitching about that, but it wasn’t.
* Fixed segfaults when using Python ffi. `step()` exists in both `GLIBC` and Powell's code. The FFI needs `GLIBC` to work.
* Removed `#FIXED_NUM_PLANES`, which was always set to same value.

## 17/11/1
* Started to move to interface.
* Deleted `dynamic_character_t` type from 3DO code, as it's no longer needed.

## 17/11/3
* Updated all 3DO types in `test_interface` to use `char` instead of `dynamic_char`.
* Changed name of `ukk.checkp.c` to `ukkCheckPoint.c`.
* All code compiles including `test_interface`.

## 17/11/6
* Removed `MAX_STR`, but had to add it back in, as two array allocations (`cost_vector` and `state_vector`) rely on it, as well as the global `MAX_COST`.
* `test_interface` kept failing, either segfaulting or assertion errors. Step I took:
    * Built a separate file, `test_c_only.c` to run `ukkCommon` without the C interface.
    * Realized the problem had to do with `align_io`, which has `elem_t`, whereas Powell’s code has `char`.
    * Okay, so I can't convert from `int` to `char`. That conversion in C only allows for ASCII characters, so 33–126, inclusive. Can I just convert to `int`?
    * Gap character was hard coded as `'-'`. That's now been changed to `(1 << alphabetSize) - 1`.
    * Added `gap_char_g` global.

## 17/11/7–8
* Finished converting char strings to int arrays.


## 17/11/9
* First value in any alphabet is \\(1 = 2^{0}\\), i.e. first bit set.
* Set gap char, which is still global, to `1 << alphabetsize - 1`.
* To get lengths of output characters need to return their values, which means passing a pointer to a data structure. Moving outputs to `characters_t*` since it has all the fields I need. I could use `alignIO_t`, but this gives me the option to move to a smaller type, like `unsigned short` later, to save space. At the same time, it would be cheaper computationally to use `alignIO_t`. That should be an easy change in future.
* Changed input type of `algn_get_cost_medians_3d` to accept `characters_t*`.

## 17/11/13
* Pushed through type change on input and output arrays from `int` to `elem_t` (`unsigned int`). This was to make translation from `align_io` to `characters_t` easier.
* In conjunction with that, was forced to use `memcpy` rather than just changing pointers on that translation.
* Cast several `void *` values in `ukkCommon.h` to `char *` to get rid of annoying warnings.
    * In `allocFinal`: `flag`, `top` and `block`.
    * In `getPtr` in `return` statement.
    * In `getPtr`: `memset`
* Renamed `Astr`, etc., `Aidx`, etc. and `Alen`, etc. to `seq1`, `lenSeq1` and `idxSeq1`, respectively.
* Added new fn to reverse `elem_t` arrays.
* Cleared warnings in both `gcc` and `clang`

## 17/11/14
* Removed dead code
* Updated `algn_3d_powell` to accept `characters_t` as input
* Finally resolved annoying valgrind errors caused by bad indices in prints in `ukkCheckPoint.c` -> `traceBack()`.
* Started getting actual sequences back from Powell. Actually, they were already coming back, but the lengths were set to 0, so they weren't getting copied correctly, and neither were gapped or ungapped getting computed.

## 17/11/16–17
* Issue with short gapped and ungapped chars was because of copying to/from align_io. 2D necessitates opening gap removal. 3D doesn't.

## 18/03/16
* Completely valgrinded code to deal with memory leaks.
* Almost all leaks were in `interface.c`.

## TODOs:
- [x] What to do with gap character
- [ ] Decide where to disambiguate. If on C side use builtin: `__builtin_clz`, which only works on unsigned
- [x] Send `characters_t` as input as well as output to `algn_3d_powell`
- [x] Get actual sequences back from 3DO alignment
- [x] Update Python test harness
- [ ] Make sure my casting, from above, doesn't cause any problems.
- [ ] Consider moving all types to common library, as they're used all over.
- [x] Gapped and ungapped outputs are too short. Why?
