# An error in an array's index or length operand is reported as itself, once,
# not replaced by "invalid array index type".
var a: array[zzz, int]
var b: array[sizeof(yyy), int]
var c: array[1 + xxx, int]

# An index type that is no ordinal type keeps its own message:
# an enum with holes is not ordinal (doc/language.md, Enumerations).
type
  Holey = enum
    h1 = 1, h5 = 5
var d: array[Holey, int]
