# An implicit generic (a type class as a parameter type, no `[T]` section)
# used to pass sem and crash the backend with "type expected" (#3135).
proc p(x: SomeInteger): int = 1
proc q(x: int or float) = discard
proc r[T](x: T; y: not int) = discard

# the explicit form is fine:
proc ok[T: SomeInteger](x: T): int = int(x) + 1
discard ok(3'u8)

# a template only constrains its argument:
template t(x: int or float) = discard
t(1)
