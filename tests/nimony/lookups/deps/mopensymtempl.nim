## Declares an open-symbol template plus `combine` overloads of its own, so the
## symbol choice baked at the declaration site is non-empty but incomplete.

proc combine*(x, y: int): string = "int"
proc combine*(x, y: bool): string = "bool"

template combined*(x, y: untyped): untyped = combine(x, y)
