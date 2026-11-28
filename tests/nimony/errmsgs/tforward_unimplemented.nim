# A forward declaration that no body implements is an error: it would compile
# to a routine with an empty body.
proc plain(): int
proc stop() {.noreturn.}
proc clamp(x: int): int {.ensures: 0 <= result.}
proc setOut(x: out int)
proc gen[T](x: T): T
proc noDecl(): cint {.nodecl.}
proc implemented(): int
proc implemented(): int = 1

proc user(x: int): int =
  if x < 0: stop()
  var v: int
  setOut(v)
  result = plain() + clamp(x) + v + gen(x) + implemented() + int(noDecl())
discard user(1)
