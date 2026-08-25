# An FFI callee's write through an addr-taken local must survive --opt:speed.
#
# Repro:   bin/nimony c -r --opt:speed tests/nimony/opt/topt_ffi_addr_write.nim
# Expected (see .output): 200 true
#
# hexer's funcsummary walked an importc proc's EMPTY `(stmts .)` body
# placeholder as if it were a real body, found zero effects, and attached an
# all-clear `(smry)` to e.g. `sscanf`/`time`. copyprop's stable-addr-taken
# classification then trusted the summary — the address escape "only feeds
# reads" — so the local's initializer was propagated across the call and the
# callee's store was lost (in production: every libcurl status read came back
# 0). Foreign procs now take the `markAllUnknown` path like any unseen callee.
import std/syncio

proc c_sscanf(buf: cstring; fmt: cstring): cint {.importc: "sscanf",
  header: "<stdio.h>", varargs.}
proc c_time(t: ptr clong): clong {.importc: "time", header: "<time.h>".}

proc main() =
  var viaVarargs: clong = 0
  discard c_sscanf(cstring"200", cstring"%ld", addr viaVarargs)
  var viaFixed: clong = 0
  discard c_time(addr viaFixed)
  echo viaVarargs, " ", viaFixed != 0

main()
