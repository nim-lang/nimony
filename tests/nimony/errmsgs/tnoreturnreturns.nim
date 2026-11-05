# A `{.noreturn.}` body that can reach its end is rejected: every call site
# trusts the pragma, so a guard `if i >= a.len: fail()` would otherwise prove
# `a[i]` and drop its bounds check (nim-lang/nimony#2858).
import std/syncio

proc fallsOff() {.noreturn.} =
  echo "fail returned"

proc returnsEarly(x: int) {.noreturn.} =
  if x > 0: return
  quit 1

proc oneBranchFallsThrough(x: int) {.noreturn.} =
  if x > 0: quit 1

# These never return and are accepted.
proc quits() {.noreturn.} = quit 1
proc raises() {.noreturn, raises.} = raise Failure
proc loops() {.noreturn.} =
  while true: discard
proc allBranchesEnd(x: int) {.noreturn.} =
  if x > 0: quit 1
  else: quits()

quits()
