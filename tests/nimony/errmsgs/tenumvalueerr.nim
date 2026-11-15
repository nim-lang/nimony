# An enum field value that fails to compile is an error, not a crash: the
# error takes the place of the value, so the field keeps its shape.

type
  E1 = enum
    a1 = zzz
  E2 = enum
    a2
    b2 = ord(zzz)
  E3 = enum
    a3
    b3 = 1 + zzz
  E4 = enum
    a4 = (zzz, "x")
  E5 = enum
    a5 = 1.5
  E6 {.pure.} = enum
    a6 = 0
    b6 = 5
    c6 = zzz
