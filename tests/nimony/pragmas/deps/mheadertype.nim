# An imported scalar type whose header nothing else in the program includes.
# `Port` never appears in an object here, only in signatures and locals.
# `<signal.h>` is standard C, so this compiles on every target.
type
  Port* {.importc: "sig_atomic_t", header: "<signal.h>".} = distinct int32

proc toPort*(x: int): Port {.inline.} =
  let p = cast[Port](int32(x))
  result = p

proc portVal*(p: Port): int = int(cast[int32](p))
