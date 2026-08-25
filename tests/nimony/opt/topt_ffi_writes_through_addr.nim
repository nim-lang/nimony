# An `importc` callee has an EMPTY body, not an effect-free one.
#
# Repro:   bin/nimony c -r --opt:speed tests/nimony/opt/topt_ffi_writes_through_addr.nim
# Expected (see .output): 72340172838076673
#
# `funcsummary` derived the summary of every proc from its body, and an
# `importc` proc's body is `(stmts .)` — a TagLit, so the walk ran, found no
# store and no escape, and emitted `(smry (param 0 0))`: "reads nothing, writes
# nothing, lets nothing out". copyprop's read-only addr-taken classification
# (`computeStableAT`) trusts exactly that summary, so `x` stayed in `stableAT`
# across `memset(addr x, 1, 8)` and its `0` snapshot was propagated past the
# call — `x` read back as `0` at compile time while the call itself still ran
# and still wrote the memory. Only `--opt:speed` runs shoggoth, so the
# unoptimized build was right and only the optimized one lost the store
# (issue #2362).
#
# `memset` is deliberately boring: it is in libc proper on every target (no
# `-lm`, no varargs, no `time_t`-style width that differs between Posix and
# Windows), `int64*` converts to its `void*` without a diagnostic, and writing
# `1` into all eight bytes gives 0x0101010101010101 whatever the endianness.
# The local has to be 64 bits wide: a `cint` one is initialized from a
# `(suf 0 i32)` that copyprop does not propagate, so it would pass either way.

import std/syncio

proc c_memset(dest: ptr int64; val: cint; size: csize_t): pointer {.importc: "memset",
  header: "<string.h>".}

proc main() =
  var x: int64 = 0
  discard c_memset(addr x, 1, csize_t(8))
  echo x

main()
