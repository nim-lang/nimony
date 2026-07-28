import std/assertions

# issue #2098
type
  B = object
    flags: AFlags
  Aenum {.size: sizeof(uint32).} = enum
    A
  AFlags = set[Aenum]

type
  C = object
    flags: set[Cenum]
  Cenum = enum
    c0, c1, c2

var b = B(flags: {A})
b.flags.incl A
assert A in b.flags

var c = C(flags: {c0, c2})
c.flags.incl c1
assert c.flags == {c0, c1, c2}
