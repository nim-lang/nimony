
type
  E = enum
    Value1
    Value2
    Value1

# Enum field values must ascend, explicit or implied.
type
  Down = enum
    a = 2
    b = 1
  Neg = enum
    n0 = 0
    n1 = -1
  Implicit = enum
    i0 = 5, i1, i2 = 3
