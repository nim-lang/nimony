# Without `.feature: "varToverloads"` the two overloads stay ambiguous —
# verifies that the disambiguation is strictly opt-in.

proc foo(x: int): string = "T"
proc foo(x: var int): string = "varT"

var v = 0
discard foo(v)
