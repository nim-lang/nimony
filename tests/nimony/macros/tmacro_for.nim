# A macro body belongs to the plugin: the calling module's lowering passes
# must not walk it, whether the macro is called from a generic or declared
# inside an inline iterator.
import std / [syncio, macros]

macro same(s: untyped): untyped =
  for i in 0 ..< s.len:
    discard s[i]
  result = s

proc viaGeneric[T](x: T): T =
  result = same(x)

echo viaGeneric(5)

iterator it(): int =
  macro m(s: untyped): untyped =
    var i = 0
    while i < 2: inc i
    result = s
  yield m(3)

for x in it(): echo x
