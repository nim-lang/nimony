# An `importc` callee has an EMPTY body, not an effect-free one.
#
# Repro:   bin/nimony c -r --opt:speed tests/nimony/opt/topt_ffi_writes_through_addr.nim
# Expected (see .output): 200 true
#
# `funcsummary` derived the summary of every proc from its body, and an
# `importc` proc's body is `(stmts .)` — a TagLit, so the walk ran, found no
# store and no escape, and emitted `(smry (param 0 0))`: "reads nothing, writes
# nothing, lets nothing out". copyprop's read-only addr-taken classification
# (`computeStableAT`) trusts exactly that summary, so `viaFixed` stayed in
# `stableAT` across `time(addr viaFixed)` and its `0` snapshot was propagated
# past the call — `viaFixed != 0` folded to `false` at compile time while the
# call itself still ran and still wrote the memory. Same for the varargs
# actual. Only `--opt:speed` runs shoggoth, so the unoptimized build was right
# and only the optimized one lost the store (issue #2362).
#
# Both arities matter: a varargs actual is out of the summary's param range
# (which `paramMayWrite` already answers conservatively) whereas `c_time`'s
# `t` is param 0 and was answered from the bogus summary.

import std/syncio

proc c_sscanf(buf: cstring; fmt: cstring): cint {.importc: "sscanf",
  header: "<stdio.h>", varargs.}
proc c_time(t: ptr clong): clong {.importc: "time", header: "<time.h>".}

proc main() =
  var viaVarargs: clong = 0
  discard c_sscanf(cstring"200", cstring"%ld", addr viaVarargs)
  var viaFixed: clong = 0
  discard c_time(addr viaFixed)
  echo int(viaVarargs), " ", viaFixed != 0

main()
