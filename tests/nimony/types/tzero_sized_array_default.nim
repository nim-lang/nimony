# An empty `(aconstr T)` is the all-zero array. For a zero-sized `T` (length
# 0, or zero-sized elements) lengc must emit `{}`: `{0}` is an excess
# initializer, a GCC/MinGW and clang warning that is made an error here.
when defined(macosx):
  {.passC: "-Werror=excess-initializers".}
else:
  # GCC names no option for this warning: `-pedantic-errors` makes it an
  # error, `-Wno-pedantic` keeps the GNU extensions (`a[0]`) accepted.
  {.passC: "-pedantic-errors".}
  {.passC: "-Wno-pedantic".}

import std/syncio

type
  O = object
    a: array[0, int]
  T1 = tuple[x: array[0, int], y: int]

proc f0(a: array[0, int]): int = a.len
proc f1(a: array[0, string]): int = a.len
proc f2(a: array[3, array[0, int]]): int = a.len + a[2].len
proc f3(a: array[2, array[2, array[0, int]]]): int = a.len + a[1].len
proc f4(a: array[2, O]): int = a.len + a[1].a.len
proc f5(a: array[2, T1]): int = a.len + a[1].y
proc f6(a: array[4, int]): int = a.len + a[0] + a[3]

echo f0(default(array[0, int]))
echo f1(default(array[0, string]))
echo f2(default(array[3, array[0, int]]))
echo f3(default(array[2, array[2, array[0, int]]]))
echo f4(default(array[2, O]))
echo f5(default(array[2, T1]))
echo f6(default(array[4, int]))
