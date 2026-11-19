# An untyped `const` in a macro with an `untyped` parameter (#3044 item 12).
import std/[syncio, macros]

macro twice(n: untyped): untyped =
  const factor = 2
  result = newIntLitNode(n.intVal * factor)

echo twice(21)
