# An imported scalar type whose header nothing else in the program includes.
# `Port` never appears in an object here, only in signatures and locals.
type
  Port* {.importc: "in_port_t", header: "<netinet/in.h>".} = distinct uint16

proc toPort*(x: int): Port {.inline.} =
  let p = cast[Port](uint16(x))
  result = p

proc portVal*(p: Port): int = int(cast[uint16](p))
