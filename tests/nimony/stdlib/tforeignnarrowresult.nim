# A narrow integer returned by FOREIGN code must be sign/zero-extended by the
# caller: neither Win64 nor SysV defines rax's upper bits for it. The native
# backend used to keep `MulDiv`'s -3 as 0x00000000FFFFFFFD (#2852), and
# `dirExists` saw a missing path's `INVALID_FILE_ATTRIBUTES` as all bits set.

import std / [syncio, os]

when defined(windows):
  proc mulDiv(a, b, c: int32): int32 {.stdcall, dynlib: "kernel32", importc: "MulDiv".}
else:
  proc mulDiv(a, b, c: int32): int32 = int32(int64(a) * int64(b) div int64(c))

proc main =
  let m = mulDiv(-6'i32, 1'i32, 2'i32)
  echo m
  echo m == -3'i32
  echo m < 0'i32
  echo mulDiv(-6'i32, 1'i32, 2'i32) == -3'i32
  echo dirExists("definitely-missing-dir-2852")
  echo fileExists("definitely-missing-file-2852")

main()
