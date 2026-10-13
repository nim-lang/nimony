# Without `.feature: "staticOverloads"` two candidates that differ only in
# whether a parameter is `static` are ambiguous: a literal `3` (type `int`)
# does not match `static[int]` better than `int`.

proc f(x: static[int]): string = "static"
proc f(x: int): string = "int"

discard f(3)
