# a qualified non-routine callee is reported, not an assertion
type E = enum a, b
discard E.a(1)
type E2 = enum a, c
discard tqualified_noncallable.a(1)
