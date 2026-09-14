# `nimparser`: a plugin turns Nim source text into code.
import std / syncio

template parsed(mode: untyped; code: string) {.plugin: "deps/mparsenim".}
template parsedExpr(mode: untyped; code: string): int {.plugin: "deps/mparsenim".}

parsed stmt, """
proc double(x: int): int = x * 2
var total = 0
for i in 1..3:
  total += double(i)
echo total
"""

echo parsedExpr(expr, "double(20) + 2")
